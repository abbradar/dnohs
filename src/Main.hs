import qualified Data.ByteString.Lazy.Char8 as BL

import Haskell.Parser
--import Haskell.Desugar
--import Core.Monad
--import Core.Rename

main :: IO ()
main = do
   input <- BL.getContents
   expr <- either fail return $ parseHaskell input
   putStrLn $ show expr
   --let nrl = desugar expr

    --putStrLn $ show nrl
    --let res' = runCompiler $ do
    --      renamed <- rename nrl
    --      return renamed
    --res <- either (fail . show) return res'
    --putStrLn $ show res
