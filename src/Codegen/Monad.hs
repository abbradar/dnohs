module Codegen.Monad
       (
       ) where

import Control.Monad.Trans.Class
import Control.Monad.Writer
import LLVM.General.AST

import Core.Unique

newtype GeneratorT m a = GeneratorT { runGeneratorT' :: WriterT [Definition] m a }
                  deriving (Functor, Applicative, Monad)

class MonadGenerator m where
  newDefinition :: Definition -> m ()

instance MonadGenerator (GeneratorT m) where
  newDefinition = GeneratorT . tell . pure

instance MonadTrans GeneratorT where
  lift = GeneratorT

runGeneratorT :: UniqueT m a -> m a
runGeneratorT = flip evalStateT 0 . runUniqueT'

instance MonadUnique m => MonadUnique (StateT s m) where
  uid = lift uid

instance MonadUnique m => MonadUnique (ReaderT s m) where
  uid = lift uid

instance MonadUnique m => MonadUnique (WriterT s m) where
  uid = lift uid

instance MonadUnique m => MonadUnique (ExceptT e m) where
  uid = lift uid


type Generator = UniqueT (Writer [Definition])
