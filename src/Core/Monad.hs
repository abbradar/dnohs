{-| The compiler's monad stack. -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Core.Monad
       ( CompError(..)
       , Compiler
       , throwCError
       , uid
       , runCompiler

       -- Re-exports
       , MonadError(..)
       ) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Except

import Core.Types

-- TODO: improve error reporting!
data CompError = CompError Pos String
               deriving (Show, Eq)

-- | Main compiler monad. It supports getting new unique variable names
--   and failing with errors.
--
--   TODO: Replace with MyExceptT with applicative-based multiple errors,
--         semi-ReaderT with Pos (possibly explicit?).
newtype Compiler a = Compiler { runCompiler' :: StateT Integer (Except CompError) a }
                       deriving (Functor, Applicative, Monad)

instance MonadError CompError Compiler where
  throwError = Compiler . throwError
  catchError m e = Compiler $ catchError (runCompiler' m) (runCompiler' . e)

throwCError :: Pos -> String -> Compiler a
throwCError = curry $ throwError . uncurry CompError

uid :: Compiler Integer
uid = Compiler $ state $ \i -> (i, succ i)

runCompiler :: Compiler a -> Either CompError a
runCompiler = runExcept . flip evalStateT 0 . runCompiler'
