module Data.IndexedSet
       ( IndexKey(..)
       , SplitKey(..)
         
       , IndexedSet
       , empty
       , null
       , (!)
       , lookup
       , insert
       , delete
       , itraverseSet
       , fromList
       ) where

import Control.Arrow
import Data.Proxy
import Data.Map (Map)
import qualified Data.Map as M
import Data.Default
import Control.Lens

class IndexKey k a where
  toIndex :: a -> k
  default toIndex :: SplitKey k a => a -> k
  toIndex = fst . view splitKey

class IndexKey k a => SplitKey k a where
  type WithoutKey k a
  splitKey :: Iso' a (k, WithoutKey k a)

newtype IndexedSet k a = ISet (Map k a)
                        deriving (Show, Eq, Default)

_indexedSet :: Lens (IndexedSet k a) (IndexedSet k' a') (Map k a) (Map k' a')
_indexedSet = lens (\(ISet x) -> x) ISet

type instance Index (IndexedSet k a) = k
type instance IxValue (IndexedSet k a) = WithoutKey k a

instance SplitKey k a => Ixed (IndexedSet k a) where
  ix k = _indexedSet . ix k . splitKey . _2

instance SplitKey k a => At (IndexedSet k a) where
  at k = _indexedSet . at k . lens (fmap $ snd . view splitKey) (const $ fmap $ view (from splitKey) . (k, ))

(!) :: Ord k => IndexedSet k a -> k -> a
(ISet m) ! k = m M.! k

infixl 9 !

empty :: IndexedMultiSet k a
empty = def

null :: IndexedMultiSet k a -> Bool
null (ISet m) = M.null m

lookup :: Ord k => k -> IndexedMultiSet k a -> [a]
lookup k (ISet m) = k `M.lookup` m

insert :: (Ord k, IndexKey k a) => a -> IndexedSet k a -> IndexedSet k a
insert a (ISet m) = ISet $ M.insert (toIndex a) a m

delete :: (Ord k, IndexKey k a) => a -> IndexedSet k a -> IndexedSet k a
delete a (ISet m) = ISet $ M.delete (toIndex a) m

itraverseSet :: (SplitKey k a, SplitKey k b) => IndexedTraversal k (IndexedSet k a) (IndexedSet k b) (WithoutKey k a) (WithoutKey k b)
itraverseSet = _indexedSet . itraverse . splitKey . _2

fromList :: (Ord k, IndexKey k a) => [a] -> IndexedSet k a
fromList = ISet . M.fromList . map (\x -> (toIndex x, x))
