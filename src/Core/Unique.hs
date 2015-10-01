module Core.Unique
       ( UniqueT
       , MonadUnique(..)
       , genTemp
       , runUniqueT
       ) where

import Control.Monad.Trans.Class
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except

import Core.Types

newtype UniqueT m a = UniqueT { runUniqueT' :: StateT Int m a }
                 deriving (Functor, Applicative, Monad)

class Monad m => MonadUnique m where
  uid :: m Int

instance Monad m => MonadUnique (UniqueT m) where
  uid = UniqueT $ state $ \i -> (i, succ i)

instance MonadTrans UniqueT where
  lift = UniqueT . lift

instance MonadReader r m => MonadReader r (UniqueT m) where
  ask = lift ask
  local f = UniqueT . local f . runUniqueT'

instance MonadWriter w m => MonadWriter w (UniqueT m) where
  tell = lift . tell
  listen = UniqueT . listen . runUniqueT'
  pass = UniqueT . pass . runUniqueT'

instance MonadError e m => MonadError e (UniqueT m) where
  throwError = lift . throwError
  catchError m e = UniqueT $ catchError (runUniqueT' m) (runUniqueT' . e)

genTemp :: (MonadUnique m, TempVar a) => m a
genTemp = tempNum <$> uid

runUniqueT :: Monad m => UniqueT m a -> m a
runUniqueT = flip evalStateT 0 . runUniqueT'

instance MonadUnique m => MonadUnique (StateT s m) where
  uid = lift uid

instance MonadUnique m => MonadUnique (ReaderT s m) where
  uid = lift uid

instance (Monoid s, MonadUnique m) => MonadUnique (WriterT s m) where
  uid = lift uid

instance MonadUnique m => MonadUnique (ExceptT e m) where
  uid = lift uid
