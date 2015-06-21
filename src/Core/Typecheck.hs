module Core.Typecheck
       ( QName(..)
       , QType
       , QTypes
       , QExpr
       , QMeta
       , QPat
       , Renamed
       ) where

import Data.Map (Map)
import qualified Data.Map as M

import Core.Types
import Core.Monad

type QType = Type Name Name Pos
type QTypes = Types QName Name Pos
type QExpr = Expr QName Name QMeta
type QMeta = (Pos, Maybe QType)
type QPat = Pat QName Name QMeta
type Renamed = Program QName Name QName Name Pos QMeta

data QName = QName Name Int
           deriving (Show, Eq, Ord)

instance TempVar QName where
  tempNum = QName "var"

{-
data Kind = Star
          | KFun Kind Kind
          deriving (Show, Eq)

data UKind = UStar
           | UKFun UKind UKind
           | AnyKind
           deriving (Show, Eq)

data KScrut = KSVar (TypeVar QName)
            | KSType (TypeLit Name)
            deriving (Show, Eq)

type KContext = Map KScrut UKind

type KindCheck = StateT (KContext, NTypes Pos) Compiler

kindUnify :: Pos -> KScrut -> UKind -> KindCheck ()
kindUnify pos name k = do
  k' <- getKind name
  r <- un k k'
  modify $ first $ M.insert name r

  where un AnyKind k = return k
        un k AnyKind = return k
        un (UKFun k1 k2) (UKFun k1' k2') = UKFun <$> un k1 k1' <*> un k2 k2'
        un UStar UStar = return UStar
        un k1 k2 = throwCError pos "Cannot unify kinds"

getKind :: KScrut -> KindCheck UKind
getKind n = do
  (ctx, _) <- get
  return $ case M.lookup n ctx of
   Nothing -> AnyKind
   Just k -> k

kindCheck :: NTypes Pos -> Compiler (NTypes (Pos, Kind))
kindCheck typs = do
  ctx <- fst <$> execStateT next (M.empty, typs)
  undefined

  where next = do
          (_, ts) <- get
          case ts ^@? I.itraverseSet of
           Just (n, _) -> deriveType n
           Nothing -> return ()

        deriveType n = do
          (_, ts) <- get
          modify $ second $ I.delete n
          case I.lookup n ts of
           Just (Ann _ (TyDecl _ vars constrs)) -> do
             mapM_ (\(Ann _ (TyCon _ pars)) -> mapM_ fromP pars) constrs
             rs <- mapM getKind $ map (\v -> Ann TApp ( vars
             kindUnify (KSType n) $ foldr UKFun UStar rs
           Nothing -> return ()

        fromP var (Ann _ (TApp (TNVar v) apps)) = do
          mapM_ fromP apps
          rs <- mapM (getKind . 
          kindUnify (KSVar v) $ foldr 
          
-}
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
