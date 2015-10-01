module Core.Pretty
       ( Pretty(..)
       , Doc
       , Complex(..)
       , br
       , printDoc
       , printPretty
       , showP
       ) where

import Prelude hiding ((<$>), exp)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Text.PrettyPrint.Leijen.Text

class Complex a where
  complex :: a -> Bool

br :: (Pretty a, Complex a) => a -> Doc
br a
  | complex a = parens (pretty a)
  | otherwise = pretty a

instance Pretty Text where
  pretty = pretty . TL.fromStrict

printDoc :: Doc -> IO ()
printDoc a = do
  putDoc a
  putStrLn "\n"

printPretty :: Pretty a => a -> IO ()
printPretty = printDoc . pretty

showP :: Pretty a => a -> String
showP = TL.unpack . displayT . renderOneLine . pretty
