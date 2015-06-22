import qualified Data.ByteString.Lazy.Char8 as BL

import Text.PrettyPrint.Leijen.Text (Doc, putDoc, pretty, vsep)
import Haskell.Parser
import Haskell.Desugar
import Haskell.Pretty
import Core.Types
import Core.Monad
import Core.Rename
import Core.Typecheck
import qualified Data.IndexedSet as I

printDoc :: Doc -> IO ()
printDoc a = do
  putDoc a
  putStrLn "\n"

main :: IO ()
main = do
   input <- BL.getContents
   expr <- either fail return $ parseHaskell input
   printDoc $ prettyHsTop expr
   nq <- either (fail . show) return $ runCompiler $ desugar expr
   printDoc $ pretty nq
   q <- either (fail . show) return $ runCompiler $ rename nq
   printDoc $ pretty q
   r <- either (fail . show) return $ runCompiler $ typecheck q
   printDoc $ vsep $ map pretty $ I.toList $ progTypes r
   return ()
