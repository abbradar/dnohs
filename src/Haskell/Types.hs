module Haskell.Types where

import Control.Lens
import qualified Text.PrettyPrint.Leijen.Text as PP

import Data.IndexedSet (IndexKey(..), SplitKey(..))
import Core.Types
import Core.Pretty

type HsType meta = NType meta
type HsTyCon meta = NTyCon meta
type HsTyDecl meta = NTyDecl meta

data HsPat' meta = HPVar Name
                 -- | HPInt Integer
                 | HPWildCard
                 | HPCon Name [HsPat meta]
                 deriving (Show, Eq)

instance (Pretty meta) => Pretty (HsPat' meta) where
  pretty (HPVar v) = pretty v
  pretty HPWildCard = "_"
  pretty (HPCon n ps) = PP.parens $ pretty n PP.<+> PP.hsep (map pretty ps)

type HsPat meta = Ann meta HsPat'

data HsLet' meta = LAnn Name (HsType meta)
                 | LBind Name [HsPat meta] (HsExpr meta)
                 deriving (Show, Eq)

instance (Pretty meta) => Pretty (HsLet' meta) where
  pretty (LAnn n t) = pretty n PP.<+> "::" PP.<+> pretty t
  pretty (LBind n [] e) = pretty n PP.<+> "=" PP.<+> pretty e
  pretty (LBind n pats e) = pretty n PP.<+> PP.hsep (map pretty pats) PP.<+> "=" PP.<+> pretty e

instance IndexKey Name (HsLet' meta)

instance SplitKey Name (HsLet' meta) where
  type WithoutKey Name (HsLet' meta) = Either (HsType meta) ([HsPat meta], (HsExpr meta))
  splitKey = iso fwd back
    where fwd (LAnn n a) = (n, Left a)
          fwd (LBind n a b) = (n, Right (a, b))
          back (n, Left a) = LAnn n a
          back (n, Right (a, b)) = LBind n a b

type HsLet meta = Ann meta HsLet'

type HsAlts meta = [(HsPat meta, HsExpr meta)]

data HsExpr' meta = HLet [HsLet meta] (HsExpr meta)
                  | HCase (HsExpr meta) (HsAlts meta)
                  | HVar Name
                  | HLit Name
                  | HInt Integer
                  | HApp (HsExpr meta) (HsExpr meta)
                  | HTyAnn (HsType meta) (HsExpr meta)
                  deriving (Show, Eq)

instance (Pretty meta) => Pretty (HsExpr' meta) where
  pretty (HVar v) = pretty v
  pretty (HLit l) = pretty l
  pretty (HInt i) = pretty i
  pretty (HLet lets expr) = "let" PP.<+> PP.align (PP.vsep $ map pretty lets) PP.<$> "in" PP.<+> pretty expr
  pretty (HCase e alts) = "case" PP.<+> pretty e PP.<+> "of" PP.<$> PP.nest 2 (PP.vsep $ map palt alts)
    where palt (pat, exp') = pretty pat PP.<+> "->" PP.<+> pretty exp'
  pretty (HApp a b) = br a PP.<+> br b
  pretty (HTyAnn n t) = pretty n PP.<+> "::" PP.<+> pretty t

instance Complex (HsExpr' meta) where
  complex (HVar _) = False
  complex (HLit _) = False
  complex (HInt _) = False
  complex (HLet _ _) = True
  complex (HCase _ _) = True
  complex (HApp _ _) = True
  complex (HTyAnn _ _) = True

type HsExpr meta = Ann meta HsExpr'

data HsTop meta = TELet (HsLet meta)
                | TEData (HsTyDecl meta)
                deriving (Show, Eq)

instance (Pretty meta) => Pretty (HsTop meta) where
  pretty (TELet l) = pretty l
  pretty (TEData d) = pretty d

type HsTopsP = [HsTop Pos]

prettyHsTop :: Pretty meta => [HsTop meta] -> Doc
prettyHsTop = PP.vsep . map pretty

makePrisms ''HsTop

makePrisms ''HsLet'
