module Core.Pretty where

import Prelude hiding ((<$>), exp)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Text.PrettyPrint.Leijen.Text

import qualified Data.IndexedSet as I

import Core.Types
import Core.Rename
import Core.Typecheck
import Core.Typed

instance Pretty Text where
  pretty = pretty . TL.fromStrict

instance Pretty Pos where
  pretty (Pos l c) = "Pos" <+> pretty (l, c)

instance (Pretty meta, Pretty a) => Pretty (Ann' meta a) where
  pretty (Ann meta a)
    | emp == "()" = pretty a
    | otherwise = align $ brackets m <$$> pretty a
    where m = pretty meta
          emp = displayT $ renderOneLine m

instance (Pretty var, Pretty lit, Pretty meta) => Pretty (Expr' var lit meta) where
  pretty (Var var) = pretty var
  pretty (Lit lit) = pretty lit
  pretty (Abs var exp) = parens $ "λ" <> pretty var <> "." </> pretty exp
  pretty (App a b) = pretty a <+> pretty b
  pretty (Let name e a) = "let" <+> pretty name <+> "=" <+> pretty e <+> "in" <+> pretty a
  pretty (Case exp alts) = "case" <+> pretty exp <+> "of" <$> indent 2 (vsep $ map palt alts)
    where palt (pat, exp') = pretty pat <+> "->" <+> pretty exp'

instance (Pretty var, Pretty lit, Pretty meta) => Pretty (Pat' var lit meta) where
  pretty (PVar var) = pretty var
  pretty (PCon n ps) = parens $ pretty n <+> hsep (map pretty ps)

instance (Pretty var, Pretty lit, Pretty meta) => Pretty (TyDecl' var lit meta) where
  pretty (TyDecl lit vars []) = "data" <+> pretty lit <+> hsep (map pretty vars)
  pretty (TyDecl lit vars (c:cons)) = pretty (TyDecl lit vars [] :: TyDecl' var lit meta)
                                      <+> align (vsep $ ("=" <+> pretty c) : map (("|" <+>) . pretty) cons)

instance (Pretty var, Pretty lit, Pretty meta) => Pretty (TyCon' var lit meta) where
  pretty (TyCon n vs) = pretty n <+> hsep (map pretty vs)

instance (Pretty var, Pretty lit, Pretty meta) => Pretty (Type' var lit meta) where
  pretty (TVar n) = pretty n
  pretty (TLit n) = pretty n
  pretty TFun = "(->)"
  pretty (TApp a b) = "(" <> pretty a <+> pretty b <> ")"

instance Pretty NQName where
  pretty (NQName n) = pretty n
  pretty (NQTemp i) = "var_" <> pretty i

instance Pretty QName where
  pretty (QName n i) = pretty n <> "_" <> pretty i

instance (Pretty var, Pretty lit, Pretty tvar, Pretty tlit, Pretty meta, Pretty tmeta) =>
         Pretty (Program var lit tvar tlit meta tmeta) where
  pretty des = vsep (map pretty $ I.toList $ progTypes des) <$> empty <$> pretty (progExpr des)

instance Pretty meta => Pretty (Kind' meta) where
  pretty Star = "*"
  pretty (KFun a b) = "(" <> pretty a <> ")" <+> "->" <+> "(" <> pretty b <> ")"

instance (Pretty var, Pretty lit, Pretty meta) => Pretty (Scheme' var lit meta) where
  pretty (Forall vars e) = "∀" <+> hsep (map pretty vars) <+> "." <+> pretty e

instance Pretty name => Pretty (KAnn name) where
  pretty (KName name kind) = "(" <> pretty name <+> " :: " <+> pretty kind <> ")"

instance Pretty name => Pretty (TAnn name) where
  pretty (TName name kind) = "(" <> pretty name <+> " :: " <+> pretty kind <> ")"
