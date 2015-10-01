module Haskell.Desugar
       ( desugar
       ) where

import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.IndexedSet (SplitKey(..))
import qualified Data.IndexedSet as I
import Control.Lens
import Control.Monad.State

import Haskell.Types
import Core.Types
import Core.Rename
import Core.Monad
import Codegen.Builtins

type Desugar a = StateT (Set Int) Compiler a

requireTuple :: Int -> Desugar ()
requireTuple = modify . S.insert

intAnn :: Pos -> a -> Ann' NQMeta a
intAnn pos = Ann (pos, [])

ipos :: Pos
ipos = Pos 0 0

desugarPat :: HsPat Pos -> Desugar NQPat
desugarPat (Ann p e) = intAnn p <$> case e of
  HPVar v -> return $ PVar $ NQName v
  HPWildCard -> PVar <$> genTemp
  HPCon l ps -> PCon l <$> mapM desugarPat ps

desugarExpr :: HsExpr Pos -> Desugar NQExpr
desugarExpr (Ann pos (HLet lets expr)) = desugarLet pos lets <*> desugarExpr expr
desugarExpr (Ann pos (HCase expr alts)) =
  intAnn pos <$> (Case <$> desugarExpr expr <*> mapM pts alts)
  where pts (p, e) = (,) <$> desugarPat p <*> desugarExpr e
desugarExpr (Ann pos (HVar v)) = return $ intAnn pos $ Var $ NQName v
desugarExpr (Ann pos (HLit l)) = return $ intAnn pos $ Lit l
desugarExpr (Ann pos (HInt i)) = return $ Ann (pos, [Ann pos $ TLit intType]) $ Int i
desugarExpr (Ann pos (HApp exp1 exp2)) = intAnn pos <$> (App <$> desugarExpr exp1 <*> desugarExpr exp2)
desugarExpr (Ann _ (HTyAnn typ expr)) = do
  Ann (pos, anns) val <- desugarExpr expr
  return $ Ann (pos, typ : anns) val

desugarLet :: Pos -> [HsLet Pos] -> Desugar (NQExpr -> NQExpr)
desugarLet tpos defs = do
  anns <- indexVals $ defs ^..traverse.annval._LAnn

  let vals :: Map NQName NQExpr -> [(Name, (Pos, [HsPat Pos], HsExpr Pos))] -> Desugar (Map NQName NQExpr)
      vals exs [] = return exs
      vals exs ds@((name, (pos, length -> nbinds, _)):_) = do
        let nname = NQName name
            ann = maybeToList $ M.lookup name anns
            (cur, next) = span ((== name) . fst) ds
        when (nname `M.member` exs) $ throwCError pos "Duplicate let binding"

        ivars <- mapM (const genTemp) [1..nbinds]
        requireTuple nbinds
        let tname = tupleName nbinds
            iargs = foldr (\n f -> intAnn pos . Abs n . f) id ivars
            scrut = foldl (\e n -> intAnn pos $ App e (intAnn pos $ Var n)) (intAnn pos $ Lit tname) ivars
        cases <- forM cur $ \(_, (_, binds', expr')) -> do
          binds <- mapM desugarPat binds'
          expr <- desugarExpr expr'
          return (intAnn pos $ PCon tname binds, expr)
        let (Ann _ expr) = iargs $ intAnn pos $ Case scrut cases
        vals (M.insert nname (Ann (pos, ann) expr) exs) next

  defs' <- vals M.empty [ (name, (pos, pats, expr)) | Ann pos (LBind name pats expr) <- defs ]
  return $ intAnn tpos . Let defs'

tupleName :: Int -> Name
tupleName 0 = "()"
tupleName 1 = "Tup1"
tupleName n = "(" <> T.replicate (n - 1) "," <> ")"

makeTuple :: Int -> HsTyDecl Pos
makeTuple n = decl
  where name = tupleName n
        vars = map tempNum [1..n]

        ann = Ann ipos
        decl = ann $ TyDecl name vars constrs
        constrs = [ann $ TyCon name tvars]
        tvars = map (ann . TVar) vars

desugar :: HsTopsP -> Compiler Desugared
desugar defs = do
  ((tys, expr), tups) <- runStateT desugar' S.empty
  let tys' = foldr (I.insert . makeTuple) tys $ S.toList tups
      tys'' = tys' `I.union` I.map (metamap .~ ipos) builtinTypes
  return Program { progTypes = tys''
                 , progExpr = expr
                 }

  where desugar' :: Desugar (NQTypes, NQExpr)
        desugar' = do
          tys <- indexVals $ map (view splitKey) $ defs^..traverse._TEData
          exprfun <- desugarLet (Pos 0 0) $ defs^..traverse._TELet
          let iexpr = exprfun $ intAnn ipos $ Var $ NQName "main"
              prelude = M.mapKeys NQName $ M.mapWithKey (\n t -> Ann (ipos, [t & metamap .~ ipos]) $ Builtin n) builtins
              expr = Ann (ipos, [Ann ipos $ TLit intType]) $ Let prelude iexpr
          return (tys, expr)
