module Data.IndexedSet
       ( IndexKey(..)
       , SplitKey(..)
       , splitKey'

       , IndexedSet
       , (!)
       , empty
       , null
       , member
       , lookup
       , insert
       , delete
       , union
       , itraverseSet
       , map
       , imap
       , toList
       , fromList
       , keysSet
       ) where

import Prelude hiding (null, lookup, map)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
-- import qualified Data.Set as S
import Data.Default.Generics
import Control.Lens hiding (imap)

class IndexKey k a where
  toIndex :: a -> k
  default toIndex :: SplitKey k a => a -> k
  toIndex = fst . view splitKey

class IndexKey k a => SplitKey k a where
  type WithoutKey k a
  splitKey :: Iso' a (k, WithoutKey k a)
  default splitKey :: (k ~ a, WithoutKey a a ~ ()) => Iso' a (a, ())
  splitKey = iso (, ()) fst

splitKey' :: (SplitKey k1 a, SplitKey k2 b) => Iso a b (k1, WithoutKey k1 a) (k2, WithoutKey k2 b)
splitKey' = iso (^.splitKey) (^.from splitKey)

newtype IndexedSet k a = ISet (Map k a)
                        deriving (Eq, Default)

instance Show a => Show (IndexedSet k a) where
  show m = "fromList " ++ show (toList m)

_indexedSet :: Iso (IndexedSet k a) (IndexedSet k' a') (Map k a) (Map k' a')
_indexedSet = iso (\(ISet x) -> x) ISet

type instance Index (IndexedSet k a) = k
type instance IxValue (IndexedSet k a) = WithoutKey k a

instance (SplitKey k a, Ord k) => Ixed (IndexedSet k a) where
  -- ugh... types can bite!
  ix k = _indexedSet . ix k . (splitKey :: Traversal' a (k, WithoutKey k a)) . __2
    where __2 :: Lens' (a', b') b'
          __2 = _2

instance (SplitKey k a, Ord k) => At (IndexedSet k a) where
  at k = _indexedSet . at k . lns
    where lns :: Lens' (Maybe a) (Maybe (WithoutKey k a))
          lns = lens (fmap $ snd . view _splitKey) (const $ fmap $ back . (k, ))
          _splitKey :: Iso' a (k, WithoutKey k a)
          _splitKey = splitKey
          back :: (k, WithoutKey k a) -> a
          back = view (from _splitKey)

(!) :: Ord k => IndexedSet k a -> k -> a
(ISet m) ! k = m M.! k

infixl 9 !

empty :: IndexedSet k a
empty = def

null :: IndexedSet k a -> Bool
null (ISet m) = M.null m

member :: Ord k => k -> IndexedSet k a -> Bool
member k (ISet m) = k `M.member` m

lookup :: Ord k => k -> IndexedSet k a -> Maybe a
lookup k (ISet m) = k `M.lookup` m

insert :: (Ord k, IndexKey k a) => a -> IndexedSet k a -> IndexedSet k a
insert a (ISet m) = ISet $ M.insert (toIndex a) a m

delete :: (Ord k, IndexKey k a) => a -> IndexedSet k a -> IndexedSet k a
delete a (ISet m) = ISet $ M.delete (toIndex a) m

union :: Ord k => IndexedSet k a -> IndexedSet k a -> IndexedSet k a
union (ISet a) (ISet b) = ISet (a `M.union` b)

itraverseSet :: forall k a b. (SplitKey k a, SplitKey k b) => IndexedTraversal k (IndexedSet k a) (IndexedSet k b) (WithoutKey k a) (WithoutKey k b)
itraverseSet = _indexedSet .> itraversed <. (splitKey' . ___2)
  where -- I said "bite", haven't I?
        _splitKey' :: Traversal a b (k, WithoutKey k a) (k, WithoutKey k b)
        _splitKey' = splitKey'
        __2 :: Lens (a', b1) (a', b2) b1 b2
        __2 = _2
        ___2 :: Lens (k, WithoutKey k a) (k, WithoutKey k b) (WithoutKey k a) (WithoutKey k b)
        ___2 = __2

map :: (SplitKey k a, SplitKey k b) => (WithoutKey k a -> WithoutKey k b) -> IndexedSet k a -> IndexedSet k b
map = over itraverseSet

imap :: (SplitKey k a, SplitKey k b) => (k -> WithoutKey k a -> WithoutKey k b) -> IndexedSet k a -> IndexedSet k b
imap = iover itraverseSet

toList :: IndexedSet k a -> [a]
toList (ISet m) = M.elems m

fromList :: (Ord k, IndexKey k a) => [a] -> IndexedSet k a
fromList = ISet . M.fromList . fmap (\x -> (toIndex x, x))

keysSet :: IndexedSet k a -> Set k
keysSet (ISet m) = M.keysSet m
