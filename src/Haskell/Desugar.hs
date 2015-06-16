module Haskell.Desugar
       ( desugar
       ) where

import Control.Monad
import Data.IndexedSet (IndexedSet, SplitKey(..))
import qualified Data.IndexedSet as I
import Data.Set (Set)
import qualified Data.Set as S

import Haskell.Parser
import Core.Types
import Core.Rename
import Core.Monad

desugar :: [HsTop Pos] -> Compiler (IndexedSet TyLitName (HsTyDecl Pos), NQExpr Pos)
desugar defs = do
  tys <- foldM insertTy I.empty $ defs^.traverse._TEData
  

  where insertTy is ty@(Ann pos (TyDecl name _ _)) = do
          when (is `has` ix name) $ throwCError pos "Duplicate data definition"
          return $ I.insert ty is
        
