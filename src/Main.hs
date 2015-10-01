import Control.Monad.Except
import qualified Data.ByteString.Lazy.Char8 as BL
import LLVM.General.Context
import LLVM.General.Module
import LLVM.General.PrettyPrint

import Haskell.Parser
import Haskell.Desugar
import Haskell.Types
import Core.Monad
import Core.Rename
import Core.Typecheck
import Core.Pretty
import Codegen.Generate
import Codegen.Run

main :: IO ()
main = do
   input <- BL.getContents
   expr <- either fail return $ parseHaskell input
   printDoc $ prettyHsTop expr
   nq <- either (fail . show) return $ runCompiler $ desugar expr
   printPretty nq
   (q, r) <- either (fail . show) return $ runCompiler $ do
     q <- rename nq
     r <- typecheck q
     return (q, r)
   printPretty q
   printPretty r
   let mdl' = generateLLVM r
   --putStrLn $ showPretty mdl'
   e <- withContext $ \ctx -> runExceptT $ withModuleFromAST ctx mdl' $ \mdl -> do
     asm <- moduleLLVMAssembly mdl
     putStrLn asm
   case e of
    Left err -> putStrLn $ "LLVM error: " ++ err
    Right _ -> return ()
   r <- runJIT mdl'
   putStrLn $ "Result: " ++ show r
