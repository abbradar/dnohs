{-| The compiler's monad stack. -}

module Core.Monad
       ( CompError
       , Compiler
       , throwCError
       , runCompiler

       -- Re-exports
       , module Core.Unique
       , MonadError(..)
       ) where

import Control.Monad.Except

import Core.Unique
import Core.Types

-- TODO: improve error reporting!
data CompError = CompError Pos String
               deriving (Show, Eq)

-- | Main compiler monad. It supports getting new unique variable names
--   and failing with errors.
--
--   TODO: Replace with MyExceptT with applicative-based multiple errors,
--         semi-ReaderT with Pos (possibly explicit?).

newtype Compiler a = Compiler { runCompiler' :: UniqueT (Except CompError) a }
                       deriving (Functor, Applicative, Monad, MonadUnique)

instance MonadError CompError Compiler where
  throwError = Compiler . throwError
  catchError m e = Compiler $ catchError (runCompiler' m) (runCompiler' . e)

throwCError :: MonadError CompError m => Pos -> String -> m a
throwCError = curry $ throwError . uncurry CompError

runCompiler :: Compiler a -> Either CompError a
runCompiler = runExcept . runUniqueT . runCompiler'
