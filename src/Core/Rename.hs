{- | Intermediate AST used for renaming. -}
module Core.Rename
       ( NQName(..)
       , NQMeta
       , NQType
       , NQTypes
       , NQPat
       , NQExpr
       , Desugared
       , rename
       , indexVals
       ) where

import Control.Monad
import Control.Arrow
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.IndexedSet as I
import Control.Lens
import Data.Default.Generics
import qualified Text.PrettyPrint.Leijen.Text as PP

import Core.Monad
import Core.Types
import Core.Pretty
import Core.Typecheck

data NQName = NQName Name
            | NQTemp Int
            deriving (Show, Eq, Ord)

instance Pretty NQName where
  pretty (NQName n) = pretty n
  pretty (NQTemp i) = "var_" PP.<> pretty i

instance TempVar NQName where
  tempNum = NQTemp

type NQMeta = (Pos, [NQType])
type NQType = NType Pos
type NQTypes = NTypes Pos
type NQPat = Pat NQName Name NQMeta
type NQExpr = Expr NQName Name NQMeta
type Desugared = Program NQName Name Name Name Pos NQMeta

type Constrs = Map Name Int

indexVals :: ( Default f
            , Index f ~ k
            , IxValue f ~ Ann' Pos v
            , At f
            , MonadError CompError m
            ) => [(k, Ann' Pos v)] -> m f
indexVals = foldM insertTy def
  where insertTy is (k, ty@(Ann pos _)) = do
          when (has (ix k) is) $ throwCError pos "Duplicate definition"
          return $ is & at k .~ Just ty

renameVar' :: Name -> Compiler QName
renameVar' name = do
  i <- uid
  return $ QName name i

renameTypes :: NQTypes -> Compiler QTypes
renameTypes typs = typs & I.itraverseSet %%~ rn
  where rn (Ann pos (TyDeclD k vars constrs)) = do
          vars' <- mapM (\v -> (v, ) <$> renameVar' v) vars
          let subst = M.fromList vars'
          unless (length vars == M.size subst) $ throwCError pos "Duplicate type variable"
          Ann pos <$> TyDeclD k (map snd vars') <$> mapM (rnConstr subst) constrs

        rnConstr :: Map Name QName -> NTyCon Pos -> Compiler QTyCon
        rnConstr subst (Ann pos (TyCon name pars)) = Ann pos <$> TyCon name <$> mapM (rnPar subst) pars

        rnPar :: Map Name QName -> NQType -> Compiler QType
        rnPar subst (Ann pos par) = Ann pos <$> case par of
          TVar v -> case M.lookup v subst of
            Just nv -> return $ TVar nv
            Nothing -> throwCError pos "Undefined type variable"
          TLit l -> do
            unless (l `I.member` typs) $ throwCError pos "Undefined type literal"
            return $ TLit l
          TFun -> return TFun
          TApp a b -> TApp <$> rnPar subst a <*> rnPar subst b

renameType :: NQTypes -> NQType -> Compiler QType
renameType typs = chk
  where chk (Ann pos par) = Ann pos <$> case par of
          TVar v -> TVar <$> renameVar' v
          TLit l -> do
            unless (l `I.member` typs) $ throwCError pos "Undefined type literal"
            return $ TLit l
          TFun -> return TFun
          TApp a b -> TApp <$> chk a <*> chk b

buildConstrs :: NQTypes -> Compiler Constrs
buildConstrs = liftM (fmap $ view annval) . indexVals . concatMap getConstrs . I.toList
  where getConstrs (Ann _ (TyDecl _ _ constrs)) = map extr constrs
        extr (Ann pos (TyCon name vs)) = (name, Ann pos $ length vs)

type Renames = Map NQName QName

renameExpr :: Constrs -> NQTypes -> NQExpr -> Compiler QExpr
renameExpr constrs types = tr M.empty
  where rnAnn :: Pos -> [NQType] -> Compiler (a -> Ann' (Pos, [QType]) a)
        rnAnn pos anns = do
          anns' <- mapM (renameType types) anns
          return $ Ann (pos, anns')

        tr :: Renames -> NQExpr -> Compiler QExpr
        tr names (Ann (pos, anns) l) = rnAnn pos anns <*> case l of
           Lit n -> do
             unless (n `M.member` constrs) $ throwCError pos "Undefined constructor"
             return $ Lit n
           Var v -> case M.lookup v names of
             Nothing -> throwCError pos "Unresolved name"
             Just v' -> return $ Var v'
           Builtin n -> return $ Builtin n
           Int i -> return $ Int i
           Abs name e -> do
             v <- newVar name
             e' <- tr (M.insert name v names) e
             return $ Abs v e'
           App e x -> App <$> tr names e <*> tr names x
           Let es x -> do
             ns <- M.fromList <$> mapM (\k -> (k, ) <$> newVar k) (M.keys es)
             let names' = M.union ns names
             es' <- M.fromList <$> mapM (\(k, e) -> (ns M.! k, ) <$> tr names' e) (M.toList es)
             x' <- tr names' x
             return $ Let es' x'
           Case e alts -> Case <$> tr names e <*> mapM (trAlt names) alts

        newVar (NQName name) = renameVar' name
        newVar (NQTemp _) = genTemp

        trAlt :: Renames -> (NQPat, NQExpr) -> Compiler (QPat, QExpr)
        trAlt names (pat, e) = do
          (allNames, pat') <- trPat pat
          names' <- M.map (view annval) <$> indexVals allNames
          (pat', ) <$> tr (names' `M.union` names) e

        trPat :: NQPat -> Compiler ([(NQName, Ann' Pos QName)], QPat)
        trPat (Ann (pos, anns) pat) = (second <$> rnAnn pos anns) <*> case pat of
          PVar name -> do
            v <- newVar name
            return ([(name, Ann pos v)], PVar v)
          PCon cname vars -> do
            case M.lookup cname constrs of
             Nothing -> throwCError pos "Undefined constructor"
             Just len -> unless (len == length vars) $ throwCError pos "Invalid number of arguments"
            (allNames, vars') <- unzip <$> mapM trPat vars
            return (concat allNames, PCon cname vars')

rename :: Desugared -> Compiler Renamed
rename prog = do
  types <- renameTypes $ progTypes prog
  constrs <- buildConstrs $ progTypes prog
  expr <- renameExpr constrs (progTypes prog) $ progExpr prog
  return $ Program { progTypes = types
                   , progExpr = expr
                   }
