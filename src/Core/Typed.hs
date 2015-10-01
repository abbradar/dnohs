module Core.Typed where

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Lens
import GHC.Generics (Generic)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Data.IndexedSet (IndexKey(..), SplitKey(..))
import Core.Types
import Core.Pretty

data QName = QName Name Int
           deriving (Show, Eq, Ord, Generic)

instance Pretty QName where
  pretty (QName n i) = PP.pretty n PP.<> "_" PP.<> PP.pretty i

type instance NameKey QName = QName

instance IndexKey QName QName where

instance SplitKey QName QName where
  type WithoutKey QName QName = ()

instance TempVar QName where
  tempNum = QName "var"

data KAnn name = KName (Kind ()) name
           deriving (Show, Eq, Generic)

instance Pretty name => Pretty (KAnn name) where
  pretty (KName kind name) = PP.parens $ PP.pretty name PP.<+> "::" PP.<+> PP.pretty kind

type instance NameKey (KAnn name) = name

instance IndexKey name (KAnn name) where

instance SplitKey name (KAnn name) where
  type WithoutKey name (KAnn name) = Kind ()
  splitKey = iso (\(KName k n) -> (n, k)) (uncurry $ flip KName)

type KName = KAnn Name
type KQName = KAnn QName

data TAnn name = TName (Type QName Name ()) name
               deriving (Show, Eq, Generic)

instance Pretty name => Pretty (TAnn name) where
  pretty (TName typ name) = PP.parens $ PP.pretty name PP.<+> "::" PP.<+> PP.pretty typ

instance Ord name => Ord (TAnn name) where
  (TName _ a) `compare` (TName _ b) = a `compare` b

type TName = TAnn Name
type TQName = TAnn QName

type TTyCon = TyCon KQName KName ()
type TTyDecl = TyDecl KQName KName ()
type TTypes = Types KQName KName ()
type TType = Type QName Name ()
type TPat = Pat TQName TName ()
type TAlts = Alts TQName TName ()
type TExpr = Expr TQName TName ()

type Typechecked = Program TQName TName KQName KName () ()

class FreeVars a where
  ftv :: a -> Set QName

instance FreeVars a => FreeVars (Ann' meta a) where
  ftv (Ann _ a) = ftv a

instance FreeVars a => FreeVars [a] where
  ftv = foldr (S.union . ftv) S.empty

instance FreeVars (Type' QName lit meta) where
  ftv (TVar v) = S.singleton v
  ftv (TLit _) = S.empty
  ftv TFun = S.empty
  ftv (TApp a b) = ftv a `S.union` ftv b

instance FreeVars (Pat' QName lit meta) where
  ftv (PVar v) = S.singleton v
  ftv (PCon _ ps) = mconcat $ map ftv ps

instance FreeVars (Expr' QName lit meta) where
  ftv (Var v) = S.singleton v
  ftv (Lit _) = S.empty
  ftv (Builtin _) = S.empty
  ftv (Int _) = S.empty
  ftv (Abs n e) = S.delete n (ftv e)
  ftv (App a b) = ftv a `S.union` ftv b
  ftv (Let ns e) = (nfs `S.union` ftv e) S.\\ M.keysSet ns
    where nfs = mconcat $ map ftv $ M.elems ns
  ftv (Case e alts) = ftv e `S.union` falts
    where falts = mconcat $ map (\(p, ae) -> ftv ae S.\\ ftv p) alts

instance FreeVars (Pat' TQName lit meta) where
  ftv = ftv . (varmap' %~ (\(TName _ n) -> n))

instance FreeVars (Expr' TQName lit meta) where
  ftv = ftv . (varmap' %~ (\(TName _ n) -> n))
