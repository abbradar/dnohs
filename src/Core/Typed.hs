module Core.Typed where

import Data.IndexedSet (IndexedSet, IndexKey(..), SplitKey(..))
import Core.Types
import Core.Monad
import Control.Lens

data QName = QName Name Int
           deriving (Show, Eq, Ord)

type instance NameKey QName = QName

instance IndexKey a a where

instance SplitKey a a where
  type WithoutKey a a = ()
  splitKey = iso (, ()) fst

instance TempVar QName where
  tempNum = QName "var"

data KAnn name = KName name (Kind ())
           deriving (Show, Eq, Ord)

type instance NameKey (KAnn name) = name

instance IndexKey name (KAnn name) where

instance SplitKey name (KAnn name) where
  type WithoutKey name (KAnn name) = Kind ()
  splitKey = iso (\(KName n k) -> (n, k)) (uncurry KName)

type KName = KAnn Name
type KQName = KAnn QName

type TTyDecl = TyDecl KQName KName ()
type TTypes = Types KQName KName ()

type Typechecked = Program QName Name KQName KName () ()
