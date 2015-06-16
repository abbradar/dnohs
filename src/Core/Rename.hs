{- | Intermediate AST used for renaming. -}
module Core.Rename
       ( TyVarName(..)
       , TyLitName(..)
       , ValVarName(..)
       , ValLitName(..)
       , NQExpr
       , rename
       ) where

import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as M

import Core.Monad
import Core.Types

newtype TyVarName = TyVarName Name
                  deriving (Show, Eq)

newtype TyLitName = TyLitName Name
                  deriving (Show, Eq)

newtype ValLitName = ValLitName Name
                   deriving (Show, Eq)

newtype ValVarName = ValVarName Name
                   deriving (Show, Eq)

type NQExpr meta = Expr meta ValVarName ValLitName

rename :: Lambda Pos var -> Compiler (Lambda Pos var)
rename = tr M.empty
  where tr names (Ann p l) = Ann p <$> case l of
          NLVar (Right v) -> return $ LVar $ VFree v
          NLVar (Left n) -> case M.lookup n names of
            Nothing -> throwCError p $ "Unresolved name: " ++ n
            Just v -> return $ LVar $ VBound v
          NLAbs name e -> do
            i <- uid
            let v = Bound name i
            e' <- tr (M.insert name v names) e
            return $ LAbs v e'
          NLApp e x -> do
            e' <- tr names e
            x' <- tr names x
            return $ LApp e' x'
