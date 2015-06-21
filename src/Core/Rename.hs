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
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.IndexedSet as I
import Control.Lens
import Data.Default.Generics

import Core.Monad
import Core.Types
import Core.Typecheck

data NQName = NQName Name
            | NQTemp Int
            deriving (Show, Eq, Ord)

instance TempVar NQName where
  tempNum = NQTemp

type NQMeta = (Pos, Maybe NQType)
type NQType = NType Pos
type NQTypes = NTypes Pos
type NQPat = Pat NQName Name NQMeta
type NQExpr = Expr NQName Name NQMeta
type Desugared = Program NQName Name Name Name Pos NQMeta

type Constrs = Set (ValLit Name)

indexVals :: ( Default f
            , Index f ~ k
            , IxValue f ~ Ann' Pos v
            , At f
            , MonadCompiler m
            ) => [(k, Ann' Pos v)] -> m f
indexVals = foldM insertTy def
  where insertTy is (k, ty@(Ann pos _)) = do
          when (has (ix k) is) $ throwCError pos "Duplicate definition"
          return $ is & at k .~ Just ty

checkType :: NTypes a -> NQType -> Compiler ()
checkType typs = chk
  where chk (Ann _ (TVar _)) = return ()
        chk (Ann pos (TLit l)) = unless (l `I.member` typs) $ throwCError pos "Undefined type literal"
        chk (Ann _ TFun) = return ()
        chk (Ann _ (TApp a b)) = chk a >> chk b

renameTypes :: NQTypes -> Compiler QTypes
renameTypes typs = typs & I.itraverseSet %%~ rn
  where rn :: Ann Pos (TyDeclD Name Name) -> Compiler (Ann Pos (TyDeclD QName Name))
        rn (Ann pos (TyDeclD vars constrs)) = do
          vars' <- forM vars $ \v -> do
            i <- uid
            return (v, fmap (\n -> QName n i) v)
          let subst = M.fromList vars'
          unless (length vars == M.size subst) $ throwCError pos "Duplicate type variable"
          Ann pos <$> TyDeclD (map snd vars') <$> mapM (rnConstr subst) constrs

        rnConstr :: Map (TyVar Name) (TyVar QName) -> NTyCon Pos -> Compiler (TyCon QName Name Pos)
        rnConstr subst (Ann pos (TyCon name pars)) = Ann pos <$> TyCon name <$> mapM (rnPar subst) pars

        rnPar :: Map (TyVar Name) (TyVar QName) -> NQType -> Compiler (Type QName Name Pos)
        rnPar subst (Ann pos par) = Ann pos <$> case par of
          TVar v -> case M.lookup v subst of
            Just nv -> return $ TVar nv
            Nothing -> throwCError pos "Undefined type variable"
          TLit l -> do
            unless (l `I.member` typs) $ throwCError pos "Undefined type literal"
            return $ TLit l
          TFun -> return TFun
          TApp a b -> TApp <$> rnPar subst a <*> rnPar subst b

buildConstrs :: NQTypes -> Compiler Constrs
buildConstrs = liftM (S.fromList . M.keys) . indexVals . concatMap getConstrs . I.toList
  where getConstrs (Ann _ (TyDecl _ _ constrs)) = map extr constrs
        extr (Ann pos (TyCon (TyLit name) _)) = (ValLit name, Ann pos ())

type Renames = Map (ValVar NQName) (ValVar QName)

renameExpr :: Constrs -> NQTypes -> NQExpr -> Compiler QExpr
renameExpr constrs types = tr M.empty
  where tr :: Renames -> NQExpr -> Compiler QExpr
        tr names (Ann (pos, ann) l) = Ann (pos, ann) <$> do
          maybe (return ()) (checkType types) ann
          case l of
           Lit n -> do
             unless (n `S.member` constrs) $ throwCError pos "Undefined constructor"
             return $ Lit n
           Var v -> case M.lookup v names of
             Nothing -> throwCError pos "Unresolved name"
             Just v' -> return $ Var v'
           Abs name e -> do
             v <- newVar name
             e' <- tr (M.insert name v names) e
             return $ Abs v e'
           App e x -> App <$> tr names e <*> tr names x
           Case e alts -> Case <$> tr names e <*> mapM (trAlt names) alts
           
        newVar (ValVar (NQName name)) = do
          i <- uid
          return $ ValVar $ QName name i
        newVar (ValVar (NQTemp _)) = ValVar <$> genTemp
          
        trAlt :: Renames -> (NQPat, NQExpr) -> Compiler (QPat, QExpr)
        trAlt names (pat, e) = do
          (allNames, pat') <- trPat pat
          names' <- M.map (view annval) <$> indexVals allNames
          (pat', ) <$> tr (names' `M.union` names) e

        trPat :: NQPat -> Compiler ([(ValVar NQName, Ann' Pos (ValVar QName))], QPat)
        trPat (Ann (pos, ann) pat) = second (Ann (pos, ann)) <$> do
                  maybe (return ()) (checkType types) ann
                  case pat of
                   PVar name -> do
                     v <- newVar name
                     return ([(name, Ann pos v)], PVar v)
                   PCon cname vars -> do
                     unless (cname `S.member` constrs) $ throwCError pos "Undefined constructor"
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
