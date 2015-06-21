module Core.Pretty where

import Prelude hiding ((<$>))
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Text.PrettyPrint.Leijen.Text

import qualified Data.IndexedSet as I

import Core.Types
import Core.Rename
import Core.Typecheck

instance Pretty Text where
  pretty = pretty . TL.fromStrict

instance Pretty Pos where
  pretty (Pos l c) = "Pos" <+> pretty (l, c)

instance (Pretty meta, Pretty a) => Pretty (Ann' meta a) where
  pretty (Ann meta a) = align $ brackets (pretty meta) <$$> pretty a

instance (Pretty var, Pretty lit, Pretty meta) => Pretty (Expr' var lit meta) where
  pretty (Var var) = pretty var
  pretty (Lit lit) = pretty lit
  pretty (Abs var exp) = parens $ "λ" <> pretty var <> "." </> pretty exp
  pretty (App a b) = pretty a <+> pretty b
  pretty (Case exp alts) = "case" <+> pretty exp <+> "of" <$> indent 2 (vsep $ map palt alts)
    where palt (pat, exp) = pretty pat <+> "->" <+> pretty exp

instance (Pretty var, Pretty lit, Pretty meta) => Pretty (Pat' var lit meta) where
  pretty (PVar var) = pretty var
  pretty (PCon n ps) = parens $ pretty n <+> hsep (map pretty ps)

instance (Pretty var, Pretty lit, Pretty meta) => Pretty (TyDecl' var lit meta) where
  pretty (TyDecl lit vars (c:cons)) = "data" <+> pretty lit
                                      <+> hsep (map pretty vars)
                                      <+> align (vsep $ ("=" <+> pretty c) : map (("|" <+>) . pretty) cons)

instance (Pretty var, Pretty lit, Pretty meta) => Pretty (TyCon' var lit meta) where
  pretty (TyCon n vs) = pretty n <+> hsep (map pretty vs)

instance (Pretty var, Pretty lit, Pretty meta) => Pretty (Type' var lit meta) where
  pretty (TApp n []) = pretty n
  pretty (TApp n vs) = pretty n <+> hsep (map pretty vs)
  pretty (TFun a b) = pretty a <+> "->" <+> pretty b

instance (Pretty var, Pretty lit, Pretty meta) => Pretty (TyName' var lit meta) where
  pretty (TNVar n) = pretty n
  pretty (TNLit n) = pretty n

instance Pretty NQName where
  pretty (NQName n) = pretty n
  pretty (NQTemp i) = "var_" <> pretty i

instance Pretty QName where
  pretty (QName n i) = pretty n <> "_" <> pretty i

instance (Pretty var, Pretty lit, Pretty tvar, Pretty tlit, Pretty meta, Pretty tmeta) =>
         Pretty (Program var lit tvar tlit meta tmeta) where
  pretty des = vsep (map pretty $ I.toList $ progTypes des) <$> empty <$> pretty (progExpr des)
