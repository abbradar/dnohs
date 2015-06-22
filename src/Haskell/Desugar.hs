module Haskell.Desugar
       ( desugar
       ) where

import Control.Monad
import Data.Monoid
import Data.IndexedSet (IndexKey(..), SplitKey(..))
import qualified Data.IndexedSet as I
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Control.Lens
import Control.Monad.State

import Haskell.Types
import Core.Types
import Core.Rename
import Core.Monad

type Desugar a = StateT (Set Int) Compiler a

requireTuple :: Int -> Desugar ()
requireTuple = modify . S.insert

intAnn :: Pos -> a -> Ann' NQMeta a
intAnn pos = Ann (pos, Nothing)

desugarPat :: HsPat Pos -> Desugar NQPat
desugarPat (Ann p e) = intAnn p <$> case e of
  HPVar var -> return $ PVar $ fmap NQName var
  HPWildCard -> PVar <$> genTemp
  HPCon lit ps -> PCon lit <$> mapM desugarPat ps

desugarExpr :: HsExpr Pos -> Desugar NQExpr
desugarExpr (Ann _ (HLet lets expr)) = desugarLet lets <*> desugarExpr expr
desugarExpr (Ann pos (HCase expr alts)) =
  Ann (pos, Nothing) <$> (Case <$> desugarExpr expr <*> mapM pts alts)
  where pts (p, e) = (,) <$> desugarPat p <*> desugarExpr e
desugarExpr (Ann pos (HVar v)) = return $ Ann (pos, Nothing) $ Var $ fmap NQName v
desugarExpr (Ann pos (HLit v)) = return $ Ann (pos, Nothing) $ Lit v
desugarExpr (Ann pos (HApp exp1 exp2)) = Ann (pos, Nothing) <$> (App <$> desugarExpr exp1 <*> desugarExpr exp2)
desugarExpr (Ann pos (HTyAnn typ expr)) = Ann (pos, Just typ) <$> _annval <$> desugarExpr expr

desugarLet :: [HsLet Pos] -> Desugar (NQExpr -> NQExpr)
desugarLet defs = do
  anns <- indexVals $ defs ^..traverse.annval._LAnn

  let addVal :: Set (ValVar Name) -> [HsLet Pos] -> Desugar (NQExpr -> NQExpr)
      addVal _ [] = return id
      addVal exs ds@((Ann pos (LBind name (length -> nbinds) exp1)):_) = do
        when (name `S.member` exs) $ throwCError pos "Duplicate let binding"
        let ann = M.lookup name anns
            (cur, next) = span ((== name) . toIndex) ds
        forM_ cur $ \(Ann pos' (LBind _ binds _)) ->
          unless (length binds == nbinds) $ throwCError pos' "Number of arguments differ"
       
        Ann (epos, eann) expr <- case nbinds of
         0 -> do
           unless (length cur == 1) $ throwCError pos "Conflicting definitions"
           desugarExpr exp1
         _ -> do
           ivars <- mapM (const genTemp) [1..nbinds]
           requireTuple nbinds
           let tname = ValLit $ tupleName nbinds
               iargs = foldr (\n f -> intAnn pos . Abs n . f) id ivars
               scrut = foldl (\e n -> intAnn pos $ App e (intAnn pos $ Var n)) (intAnn pos $ Lit tname) ivars
           cases <- forM cur $ \(Ann _ (LBind _ binds' expr')) -> do
             binds <- mapM desugarPat binds'
             expr <- desugarExpr expr'
             return (intAnn pos $ PCon tname binds, expr)
           return $ iargs $ intAnn pos $ Case scrut cases

        let fun x = Ann (pos, ann) $ App (intAnn epos $ Abs (fmap NQName name) x) $ Ann (epos, eann) expr
        restfun <- addVal (S.insert name exs) next
        return $ restfun . fun

  addVal S.empty $ filter (has $ annval._LBind) defs

tupleName :: Int -> Name
tupleName 0 = "()"
tupleName 1 = "Tup1"
tupleName n = "(" <> T.replicate (n - 1) "," <> ")"

makeTuple :: Int -> HsTyDecl Pos
makeTuple n = decl
  where name = tupleName n
        vars = map tempNum [1..n]

        ann = Ann (Pos 0 0)
        decl = ann $ TyDecl (TyLit name) vars constrs
        constrs = [ann $ TyCon (ValLit name) tvars]
        tvars = map (ann . TVar) vars

desugar :: HsTopsP -> Compiler Desugared
desugar defs = do
  ((tys, expr), tups) <- runStateT desugar' S.empty
  let tys' = foldr (I.insert . makeTuple) tys $ S.toList tups
  return Program { progTypes = tys'
                 , progExpr = expr
                 }

  where desugar' :: Desugar (NQTypes, NQExpr)
        desugar' = do
          tys <- indexVals $ map (view splitKey) $ defs^..traverse._TEData
          exprfun <- desugarLet $ defs^..traverse._TELet
          let expr = exprfun $ intAnn (Pos 0 0) $ Var $ ValVar (NQName "main")
          return (tys, expr)
