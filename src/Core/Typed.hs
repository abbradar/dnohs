module Core.Typed where

import Data.IndexedSet (IndexKey(..), SplitKey(..))
import Core.Types
import Control.Lens

data QName = QName Name Int
           deriving (Show, Eq, Ord)

type instance NameKey QName = QName

instance IndexKey QName QName where

instance SplitKey QName QName where
  type WithoutKey QName QName = ()

instance TempVar QName where
  tempNum = QName "var"

data KAnn name = KName name (Kind ())
           deriving (Show, Eq)

type instance NameKey (KAnn name) = name

instance IndexKey name (KAnn name) where

instance SplitKey name (KAnn name) where
  type WithoutKey name (KAnn name) = Kind ()
  splitKey = iso (\(KName n k) -> (n, k)) (uncurry KName)

type KName = KAnn Name
type KQName = KAnn QName

data TAnn name = TName name (Type QName Name ())
               deriving (Show, Eq)

type TName = TAnn Name
type TQName = TAnn QName

type TTyCon = TyCon KQName KName ()
type TTyDecl = TyDecl KQName KName ()
type TTypes = Types KQName KName ()
type TType = Type QName Name ()
type TPat = Pat TQName TName ()
type TExpr = Expr TQName TName ()

type Typechecked = Program TQName TName KQName KName () ()
