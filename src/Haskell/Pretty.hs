module Haskell.Pretty where

import Prelude hiding ((<$>), exp)
import Text.PrettyPrint.Leijen.Text

import Core.Pretty ()
import Haskell.Types

instance (Pretty meta) => Pretty (HsPat' meta) where
  pretty (HPVar var) = pretty var
  pretty HPWildCard = "_"
  pretty (HPCon n ps) = parens $ pretty n <+> hsep (map pretty ps)

instance (Pretty meta) => Pretty (HsLet' meta) where
  pretty (LAnn n t) = pretty n <+> "::" <+> pretty t
  pretty (LBind n [] exp) = pretty n <+> "=" <+> pretty exp
  pretty (LBind n pats exp) = pretty n <+> hsep (map pretty pats) <+> "=" <+> pretty exp

instance (Pretty meta) => Pretty (HsExpr' meta) where
  pretty (HVar var) = pretty var
  pretty (HLit lit) = pretty lit
  pretty (HLet lets expr) = "let" <+> align (vsep $ map pretty lets) <$> "in" <+> pretty expr
  pretty (HCase exp alts) = "case" <+> pretty exp <+> "of" <$> nest 2 (vsep $ map palt alts)
    where palt (pat, exp') = pretty pat <+> "->" <+> pretty exp'
  pretty (HApp a b) = pretty a <+> pretty b
  pretty (HTyAnn n t) = pretty n <+> "::" <+> pretty t

instance (Pretty meta) => Pretty (HsTop meta) where
  pretty (TELet l) = pretty l
  pretty (TEData d) = pretty d

prettyHsTop :: Pretty meta => [HsTop meta] -> Doc
prettyHsTop = vsep . map pretty
