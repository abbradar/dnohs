module Codegen.Run where

import Control.Monad.Except
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import LLVM.General.AST (Module, Name(..))
import LLVM.General.Context
import LLVM.General.Module hiding (Module)
import LLVM.General.ExecutionEngine

foreign import ccall "dynamic" runExternal :: FunPtr (Ptr Int -> IO Int) -> Ptr Int -> IO Int

runJIT :: Module -> IO (Maybe Int)
runJIT mdl' = do
  e <- withContext $ \ctx ->
    runExceptT $
    withModuleFromAST ctx mdl' $ \mdl ->
    withMCJIT ctx Nothing Nothing Nothing Nothing $ \jit ->
    withModuleInEngine jit mdl $ \ee -> do
      mainfn <- getFunction ee (Name "main")
      case mainfn of
       Nothing -> fail "Can't find the entry point"
       Just fn -> alloca $ \res -> do
         err <- runExternal (castFunPtr fn) res
         case err of
          0 -> Just <$> peek res
          _ -> return Nothing
  case e of
   Left txt -> fail $ "LLVM error: " ++ txt
   Right r -> return r
