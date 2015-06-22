module Core.Typecheck
       ( QName(..)
       , QType
       , QTyCon
       , QTypes
       , QExpr
       , QMeta
       , QPat
       , Renamed
       , renameVar
       , renameVar'
       , typecheck
       ) where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.IndexedSet (IndexedSet)
import qualified Data.IndexedSet as I
import Control.Monad.Reader
import Control.Monad.Writer

import Core.Types
import Core.Monad
import Core.Typed

type QType = Type QName Name Pos
type QTyCon = TyCon QName Name Pos
type QTyDecl = TyDecl QName Name Pos
type QTypes = Types QName Name Pos
type QExpr = Expr QName Name QMeta
type QMeta = (Pos, Maybe QType)
type QPat = Pat QName Name QMeta
type Renamed = Program QName Name QName Name Pos QMeta

renameVar :: Name -> Compiler QName
renameVar name = QName name <$> uid

renameVar' :: Functor f => f Name -> Compiler (f QName)
renameVar' name = do
  i <- uid
  return $ fmap (\n -> QName n i) name

type SubstType lit = Type QName lit Pos
type Subst lit = Map (TyVar QName) (SubstType lit)
type Constraint lit = (SubstType lit, SubstType lit)

class Substitutable lit a where
  apply :: Subst lit -> a -> a

class FreeVars a where
  ftv :: a -> Set (TyVar QName)

instance Substitutable lit (Subst lit) where
  apply a b = M.map (apply a) b `M.union` a

instance Substitutable lit a => Substitutable lit [a] where
  apply = map . apply

instance (Substitutable lit a, Substitutable lit b) => Substitutable lit (a, b) where
  apply s (a, b) = (apply s a, apply s b)

instance Substitutable lit (SubstType lit) where
  apply s t@(Ann _ (TVar v)) = M.findWithDefault t v s
  apply _ t@(Ann _ (TLit _)) = t
  apply _ t@(Ann _ TFun) = t
  apply s (Ann pos (TApp a b)) = Ann pos $ TApp (apply s a) (apply s b)

instance FreeVars (SubstType lit) where
  ftv (Ann _ (TVar v)) = S.singleton v
  ftv (Ann _ (TLit _)) = S.empty
  ftv (Ann _ TFun) = S.empty
  ftv (Ann _ (TApp a b)) = ftv a `S.union` ftv b

occursCheck :: FreeVars a => TyVar QName -> a -> Bool
occursCheck n t = n `S.member` ftv t

unify :: Eq lit => SubstType lit -> SubstType lit -> Compiler (Subst lit)
unify (Ann _ (TApp a b)) (Ann _ (TApp a' b')) = do
  s1 <- unify a a'
  s2 <- unify (apply s1 b) (apply s1 b')
  return $ apply s2 s1
unify (Ann _ (TVar a)) t = bind a t
unify t (Ann _ (TVar a)) = bind a t
unify (Ann _ (TLit a)) (Ann _ (TLit b)) | a == b = return M.empty
unify (Ann _ TFun) (Ann _ TFun) = return M.empty
unify (Ann pos _) (Ann _ _) = throwCError pos $ "Cannot unify types"

unifyAll :: Eq lit => [Constraint lit] -> Compiler (Subst lit)
unifyAll [] = return M.empty
unifyAll ((a, b):t) = do
  s1 <- unify a b
  s2 <- unifyAll (apply s1 t)
  return $ apply s2 s1

bind :: TyVar QName -> SubstType lit -> Compiler (Subst lit)
bind n (Ann _ (TVar n')) | n == n' = return M.empty
bind n t@(Ann pos _) | occursCheck n t = throwCError pos "Infinite type"
                     | otherwise = return $ M.singleton n t

type Infer lit = WriterT [Constraint lit] Compiler

uni :: SubstType lit -> SubstType lit -> Infer lit ()
uni t1 t2 = tell [(t1, t2)]

type KType = SubstType ()
type KInfer = Infer ()
type KContext = Map (TyLit Name) (TyVar QName)
type KVContext = Map (TyVar QName) (TyVar QName)

star :: Pos -> KType
star pos = Ann pos $ TLit $ TyLit ()

kfun :: Pos -> KType -> KType -> KType
kfun pos a b = Ann pos $ TApp (Ann pos $ TApp (Ann pos TFun) a) b

kvar :: Pos -> QName -> KType
kvar pos = Ann pos . TVar

inferKType :: (Pos -> TyLit Name -> KType) -> (Pos -> TyVar QName -> KType) -> QType -> KInfer (KType)
inferKType getLit getVar = chk
  where chk (Ann pos typ) = case typ of
          TApp a b -> do
            t1 <- chk a
            t2 <- chk b
            t <- kvar pos <$> genTemp
            uni t1 $ kfun pos t2 t
            return t
          TVar v -> return $ getVar pos v
          TLit l -> return $ getLit pos l
          TFun -> return $ kfun pos (star pos) (kfun pos (star pos) (star pos))

inferKinds :: QTypes -> Compiler TTypes
inferKinds typs = do
  ctx <- liftM M.fromList $ mapM (\(Ann _ (TyDecl n@(TyLit name) _ _)) -> (n, ) <$> TyVar <$> renameVar name) $ I.toList typs
  constraints <- execWriterT $ mapM_ (inferKind ctx) $ I.toList typs
  ss <- unifyAll constraints
  return $ I.fromList $ map (applyKind ctx ss) $ I.toList typs

  where applyVar :: Functor f => Subst () -> f QName -> f KQName
        applyVar subst = fmap $ \name -> KName name $ convKind $ subst M.! TyVar name

        applyLit :: Functor f => KContext -> Subst () -> f Name -> f KName
        applyLit ctx subst = fmap $ \name -> KName name $ convKind $ subst M.! (ctx M.! TyLit name)

        applyKind :: KContext -> Subst () -> QTyDecl -> TTyDecl
        applyKind ctx subst (Ann _ (TyDecl name vars constrs)) =
          Ann () $ TyDecl (applyLit ctx subst name) (map (applyVar subst) vars) (map (applyConstr ctx subst) constrs)

        applyConstr ctx subst (Ann _ (TyCon name pars)) =
          Ann () $ TyCon (fmap (\n -> KName n (Ann () Star)) name) (map (applyPar ctx subst) pars)
        
        applyPar ctx subst = chk
          where chk (Ann _ p) = Ann () $ case p of
                  TVar v -> TVar $ applyVar subst v
                  TLit l -> TLit $ applyLit ctx subst l
                  TFun -> TFun
                  TApp a b -> TApp (chk a) (chk b)

        convKind :: KType -> Kind ()
        convKind (Ann _ p) = Ann () $ case p of
          -- by default
          TVar _ -> Star
          TLit _ -> Star
          TApp (Ann _ (TApp (Ann _ TFun) a)) b -> KFun (convKind a) (convKind b)
          _ -> error "convKind: invalid kind"

        inferKind :: KContext -> QTyDecl -> KInfer ()
        inferKind ctx (Ann pos (TyDecl name vars constrs)) = do
          let tvars = vars
              vctx = M.fromList $ zip vars tvars
          rs <- concat <$> mapM (\(Ann _ (TyCon _ pars)) -> mapM (inferPar ctx vctx) pars) constrs
          mapM_ (uni $ star pos) rs
          let typ = foldr (kfun pos . kvar pos) (star pos) tvars
              v = kvar pos $ fromJust $ M.lookup name ctx
          uni typ v

        inferPar :: KContext -> KVContext -> QType -> KInfer KType
        inferPar ctx vctx = inferKType (clookup ctx) (clookup vctx)
          where clookup m pos k = kvar pos $ fromJust $ M.lookup k m

{-kindCheck :: TTypes -> QType -> Compiler ()
kindCheck typs typ = do
  constraints <- execWriterT $ inferType typ
  where getLit name = a
          where (Ann _ (TyDecl 
        inferType = inferKType (\_ name -> typs I.!  kvar
-}
typecheck :: Renamed -> Compiler Typechecked
typecheck prog = do
  kinded <- inferKinds $ progTypes prog
  return $ Program { progTypes = kinded }

{-checkTypes :: NQTypes -> Compiler ()
checkTypes typs = mapM_ check $ I.toList typs
  where check (Ann pos (TyDecl name vars constrs)) = do
          let vars' = S.fromList vars
          unless (length vars == S.size vars') $ throwCError pos "Duplicate type variable"
          mapM_ (\(Ann _ (TyCon _ pars)) -> mapM_ (checkPar vars) pars) constrs

        checkPar vars (Ann pos (TApp n ps)) = do
          case n of
           TNVar v -> unless (v `S.member` vars) $ throwCError pos "Undefined type variable"
           TNLit n -> unless (n `I.member` typs) $ throwCError pos "Undefined type literal"
          mapM_ (checkPar vars) ps
        checkPar vars (Ann _ (TFun a b)) = checkPar vars a >> checkPar vars b

buildConstrs :: NQTypes -> Compiler Constrs
buildConstrs = liftM (I.map deann) $ indexVals $ concatMap getConstrs $ I.toList $ progTypes prog
  where getConstrs (Ann _ (TyDecl tname vars cons)) = map mkcon cons
          where ftype = TApp (Ann pos tname) $ map (\x -> Ann pos $ TApp (Ann pos $ TNVar x) []) vars
                mkcon (Ann pos (TyCon name pars)) = (name, foldr (Ann pos) ftype pars)

-}
