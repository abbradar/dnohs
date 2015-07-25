import Text.PrettyPrint.Leijen.Text (Doc, putDoc, pretty)
import Haskell.Parser
import Haskell.Desugar
import Haskell.Pretty
import Core.Monad
import Core.Rename
import Core.Typecheck
import qualified Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy.Char8 as BL

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
   (q, r) <- either (fail . show) return $ runCompiler $ do
     q <- rename nq
     r <- typecheck q
     return (q, r)
   printDoc $ pretty q
   printDoc $ pretty r
   BL.putStrLn $ JSON.encodePretty r
   
   return ()
