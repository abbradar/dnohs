{-| The compiler's monad stack. -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Core.Monad
       ( CompError
       , Compiler
       , throwCError
       , genTemp
       , MonadCompiler(..)
       , runCompiler

       -- Re-exports
       , MonadError(..)
       ) where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer

import Core.Types

-- TODO: improve error reporting!
data CompError = CompError Pos String
               deriving (Show, Eq)

-- | Main compiler monad. It supports getting new unique variable names
--   and failing with errors.
--
--   TODO: Replace with MyExceptT with applicative-based multiple errors,
--         semi-ReaderT with Pos (possibly explicit?).

newtype Compiler a = Compiler { runCompiler' :: StateT Int (Except CompError) a }
                       deriving (Functor, Applicative, Monad)

instance MonadError CompError Compiler where
  throwError = Compiler . throwError
  catchError m e = Compiler $ catchError (runCompiler' m) (runCompiler' . e)

throwCError :: MonadError CompError m => Pos -> String -> m a
throwCError = curry $ throwError . uncurry CompError

class MonadError CompError m => MonadCompiler m where
  uid :: m Int

instance MonadCompiler Compiler where
  uid = Compiler $ state $ \i -> (i, succ i)

genTemp :: (MonadCompiler m, TempVar a) => m a
genTemp = tempNum <$> uid

runCompiler :: Compiler a -> Either CompError a
runCompiler = runExcept . flip evalStateT 0 . runCompiler'

instance MonadCompiler m => MonadCompiler (StateT s m) where
  uid = lift uid

instance (Monoid s, MonadCompiler m) => MonadCompiler (WriterT s m) where
  uid = lift uid
