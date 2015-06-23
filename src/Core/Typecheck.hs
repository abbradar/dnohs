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
import qualified Data.IndexedSet as I
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Lens

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
type SubstScheme lit = Scheme QName lit Pos
type Subst lit = Map (TyVar QName) (SubstType lit)
type Constraint lit = (SubstType lit, SubstType lit)

class Substitutable lit a where
  apply :: Subst lit -> a -> a

class FreeVars a where
  ftv :: a -> Set (TyVar QName)

instance FreeVars a => FreeVars (Ann' meta a) where
  ftv (Ann _ a) = ftv a

instance FreeVars a => FreeVars [a] where
  ftv = foldr (S.union . ftv) S.empty

instance Substitutable lit a => Substitutable lit [a] where
  apply = map . apply

instance Substitutable lit (Subst lit) where
  apply a b = M.map (apply a) b `M.union` a

instance (Substitutable lit a, Substitutable lit b) => Substitutable lit (a, b) where
  apply s (a, b) = (apply s a, apply s b)

instance Substitutable lit (SubstType lit) where
  apply s t@(Ann _ (TVar v)) = M.findWithDefault t v s
  apply _ t@(Ann _ (TLit _)) = t
  apply _ t@(Ann _ TFun) = t
  apply s (Ann pos (TApp a b)) = Ann pos $ TApp (apply s a) (apply s b)

instance FreeVars (Type' QName lit meta) where
  ftv (TVar v) = S.singleton v
  ftv (TLit _) = S.empty
  ftv TFun = S.empty
  ftv (TApp a b) = ftv a `S.union` ftv b

instance Substitutable lit (SubstScheme lit) where
  apply s (Ann pos (Forall as t)) = Ann pos $ Forall as $ apply s' t
    where s' = foldr M.delete s as

instance FreeVars (Scheme' QName lit meta) where
  ftv (Forall vs t) = ftv t `S.difference` S.fromList vs

instance Substitutable Name TContext where
  apply s = M.map (apply s)

instance FreeVars TContext where
  ftv = ftv . M.elems

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

uni :: MonadWriter [Constraint lit] m => SubstType lit -> SubstType lit -> m ()
uni t1 t2 = tell [(t1, t2)]

type KType = SubstType ()
type KInfer = Infer ()
type KContext = Map (TyLit Name) (TyVar QName)
type KVContext = Map (TyVar QName) (TyVar QName)

fun :: meta -> Type var lit meta -> Type var lit meta -> Type var lit meta
fun pos a b = Ann pos $ TApp (Ann pos $ TApp (Ann pos TFun) a) b

app :: meta -> Type var lit meta -> Type var lit meta -> Type var lit meta
app pos a b = Ann pos $ TApp a b

var :: meta -> TyVar var -> Type var lit meta
var pos = Ann pos . TVar

lit :: meta -> TyLit lit -> Type var lit meta
lit pos = Ann pos . TLit

star :: Pos -> KType
star pos = lit pos $ TyLit ()

inferKType :: (Pos -> TyLit Name -> KType) -> (Pos -> TyVar QName -> KType) -> QType -> KInfer (KType)
inferKType getLit getVar topt@(Ann tpos _) = do
  r <- chk topt
  uni r $ star tpos
  return r

  where chk (Ann pos typ) = case typ of
          TApp a b -> do
            t1 <- chk a
            t2 <- chk b
            t <- var pos <$> genTemp
            uni t1 $ fun pos t2 t
            return t
          TVar v -> return $ getVar pos v
          TLit l -> return $ getLit pos l
          TFun -> return $ fun pos (star pos) (fun pos (star pos) (star pos))

ktypeToKind :: KType -> Kind ()
ktypeToKind (Ann _ p) = Ann () $ case p of
  -- by default
  TVar _ -> Star
  TLit _ -> Star
  TApp (Ann _ (TApp (Ann _ TFun) a)) b -> KFun (ktypeToKind a) (ktypeToKind b)
  _ -> error "convKind: invalid kind"

kindToKtype :: Pos -> Kind () -> KType
kindToKtype pos (Ann _ Star) = Ann pos $ TLit $ TyLit ()
kindToKtype pos (Ann _ (KFun a b)) = fun pos (kindToKtype pos a) (kindToKtype pos b)

inferKinds :: QTypes -> Compiler TTypes
inferKinds typs = do
  ctx <- liftM M.fromList $ mapM (\(Ann _ (TyDecl n@(TyLit name) _ _)) -> (n, ) <$> TyVar <$> renameVar name) $ I.toList typs
  constraints <- execWriterT $ mapM_ (inferKind ctx) $ I.toList typs
  ss <- unifyAll constraints
  return $ I.fromList $ map (applyKind ctx ss) $ I.toList typs

  where applyVar :: Functor f => Subst () -> f QName -> f KQName
        applyVar subst = fmap $ \name -> KName name $ ktypeToKind $ subst M.! TyVar name

        applyLit :: Functor f => KContext -> Subst () -> f Name -> f KName
        applyLit ctx subst = fmap $ \name -> KName name $ ktypeToKind $ subst M.! (ctx M.! TyLit name)

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

        inferKind :: KContext -> QTyDecl -> KInfer ()
        inferKind ctx (Ann pos (TyDecl name vars constrs)) = do
          let tvars = vars
              vctx = M.fromList $ zip vars tvars
          mapM_ (\(Ann _ (TyCon _ pars)) -> mapM (inferPar ctx vctx) pars) constrs
          let typ = foldr (fun pos . var pos) (star pos) tvars
              v = var pos $ fromJust $ M.lookup name ctx
          uni typ v

        inferPar :: KContext -> KVContext -> QType -> KInfer KType
        inferPar ctx vctx = inferKType (clookup ctx) (clookup vctx)
          where clookup m pos k = var pos $ fromJust $ M.lookup k m

kindCheck :: TTypes -> QType -> Compiler ()
kindCheck typs typ = do
  constraints <- execWriterT $ inferKType' typ
  void $ unifyAll constraints
  where getLit pos name = kindToKtype pos kind
          where (Ann _ (TyDecl (TyLit (KName _ kind)) _ _)) = typs I.! name
        inferKType' = inferKType getLit var

annsCheck :: TTypes -> QExpr -> Compiler ()
annsCheck typs expr = void (expr & metamap %%~ chk)
  where chk :: QMeta -> Compiler QMeta
        chk r@(_, ann) = do
          maybe (return ()) (kindCheck typs) ann
          return r

type ConstrType = Type QName Name ()
type ConstrScheme = Scheme QName Name ()
type Constrs = Map (ValLit Name) Constr
type Constr = (ConstrScheme, [ConstrType])

buildConstrs :: QTypes -> Constrs
buildConstrs = M.fromList . concatMap getConstrs . I.toList
  where getConstrs :: QTyDecl -> [(ValLit Name, Constr)]
        getConstrs (Ann _ (TyDecl tname vars constrs)) = map mkcon constrs
          where ftype :: ConstrScheme
                ftype = Ann () $ Forall vars $ foldl (\t a -> app () t (var () a)) (lit () tname) vars
                mkcon :: QTyCon -> (ValLit Name, Constr)
                mkcon (Ann _ (TyCon name pars)) = (name, (ftype, ptyps))
                  where ptyps :: [ConstrType]
                        ptyps = map (metamap %~ const ()) pars

type TInfer = ReaderT TContext (Infer Name)
type TSubst = Subst Name
type TSubstType = SubstType Name
type TSubstScheme = SubstScheme Name
type TContext = Map (ValVar QName) TSubstScheme

liftVar :: ValVar a -> TyVar a
liftVar (ValVar a) = TyVar a

instantiate ::  TSubstScheme -> TInfer TSubstType
instantiate (Ann pos (Forall as t)) = do
    as' <- mapM (const $ var pos <$> genTemp) as
    let s = M.fromList $ zip as as' :: TSubst
    return $ apply s t

generalize :: TContext -> TSubstType -> TSubstScheme
generalize env t@(Ann pos _)  = Ann pos $ Forall as t
    where as = S.toList $ ftv t `S.difference` ftv env

inferType :: Constrs -> QExpr -> Compiler TExpr
inferType constrs expr = do
  constraints <- execWriterT (runReaderT (inferExpr expr) M.empty)
  cs <- unifyAll constraints
  return $ applyExpr cs expr

    where getSubst :: TSubst -> ValVar QName -> ValVar TQName
          getSubst subst (ValVar v) = ValVar (TName v (subst M.! TyVar v & metamap %~ const ()))

          getConstr :: ValLit Name -> ValLit TName
          getConstr vl@(ValLit l) = ValLit (TName l t)
            where (Ann _ (Forall _ t), _) = constrs M.! vl

          applyExpr :: TSubst -> QExpr -> TExpr
          applyExpr subst (Ann _ p) = Ann () $ case p of
            Var v -> Var $ getSubst subst v
            Lit l -> Lit $ getConstr l
            Abs v e -> Abs (getSubst subst v) (applyExpr subst e)
            App e1 e2 -> App (applyExpr subst e1) (applyExpr subst e2)
            Let v e1 e2 -> Let (getSubst subst v) (applyExpr subst e1) (applyExpr subst e2)
            Case sc as -> Case (applyExpr subst sc) (map (\(pt, e) -> (applyPat subst pt, applyExpr subst e)) as)

          applyPat :: TSubst -> QPat -> TPat
          applyPat subst (Ann _ p) = Ann () $ case p of
            (PVar v) -> PVar $ getSubst subst v
            (PCon v ps) -> PCon (getConstr v) (map (applyPat subst) ps)

          inEnv :: ValVar QName -> TSubstScheme -> TInfer a -> TInfer a
          inEnv name s = local (M.insert name s)

          constrFun :: Pos -> Constr -> TInfer TSubstType
          constrFun pos (Ann _ (Forall vars ftype), ptyps) =
            instantiate $ Ann pos $ Forall vars (foldr (fun ()) ftype ptyps & metamap %~ const pos)

          unifyAnn ann typ = maybe (return ()) (uni typ) ann
  
          inferPat :: QPat -> TInfer (TContext, TSubstType)
          inferPat (Ann (pos, ann) p) = do
            r@(_, typ) <- case p of
              PVar v -> do
                let tv = var pos $ liftVar v
                return (M.singleton v $ Ann pos $ Forall [] tv, tv)
              PCon name pats -> do
                let (ctyp'@(Ann _ (Forall vars _)), ts) = constrs M.! name
                ctyp <- instantiate (ctyp' & metamap %~ const pos)
                ctxs <- forM (zip pats ts) $ \(pt, t) -> do
                  (ctx, typ) <- inferPat pt
                  t' <- instantiate $ Ann pos $ Forall vars (t & metamap %~ const pos)
                  uni typ t'
                  return ctx
                let ctx = foldr1 M.union ctxs
                return (ctx, ctyp)
            unifyAnn ann typ
            return r

          inferExpr :: QExpr -> TInfer TSubstType
          inferExpr (Ann (pos, ann) expr') = do
            typ <- case expr' of
              Var x -> do
                env <- ask
                instantiate $ env M.! x
              Lit l -> do
                let c = constrs M.! l
                constrFun pos c
              Abs x e -> do
                let tv = var pos $ liftVar x
                t <- inEnv x (Ann pos $ Forall [] tv) $ inferExpr e
                return $ fun pos tv t
              App e1 e2 -> do
                t1 <- inferExpr e1
                t2 <- inferExpr e2
                t <- var pos <$> genTemp
                uni t1 (fun pos t2 t)
                return t
              Let x ex e -> do
                tex <- inferExpr ex
                env <- ask
                t <- inEnv x (generalize env tex) $ inferExpr e
                -- only to find them easier
                uni (var pos $ liftVar x) tex
                return t
              Case scr alts -> do
                te <- inferExpr scr
                t <- var pos <$> genTemp
                forM_ alts $ \(pat, e) -> do
                  (ctx, typ) <- inferPat pat
                  uni te typ
                  ta <- local (M.union ctx) $ inferExpr e
                  uni ta t
                return t
            unifyAnn ann typ
            return typ
                
typecheck :: Renamed -> Compiler Typechecked
typecheck prog = do
  kinded <- inferKinds $ progTypes prog
  annsCheck kinded $ progExpr prog
  let constrs = buildConstrs $ progTypes prog
  typed <- inferType constrs $ progExpr prog
  return $ Program { progTypes = kinded
                   , progExpr = typed
                   }
