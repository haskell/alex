{-# LANGUAGE CPP #-}
module Map (
   Map,
   member, lookup, findWithDefault,
   empty,
   insert, insertWith,
   delete,
   union, unionWith, unions,
   mapWithKey,
   elems,
   fromList, fromListWith,
   toAscList
) where

#if __GLASGOW_HASKELL__ >= 603
import Data.Map
import Prelude ()
#else
import Data.FiniteMap
import Prelude hiding ( lookup )

type Map k a = FiniteMap k a

member :: Ord k => k -> Map k a -> Bool
member = elemFM

lookup :: Ord k => k -> Map k a -> Maybe a
lookup = flip lookupFM

findWithDefault :: Ord k => a -> k -> Map k a -> a
findWithDefault a k m = lookupWithDefaultFM m a k

empty :: Map k a
empty = emptyFM

insert :: Ord k => k -> a -> Map k a -> Map k a
insert k a m = addToFM m k a

insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith c k a m = addToFM_C c m k a

delete :: Ord k => k -> Map k a -> Map k a
delete = flip delFromFM

union :: Ord k => Map k a -> Map k a -> Map k a
union = flip plusFM

unionWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith c l r = plusFM_C c r l

unions :: Ord k => [Map k a] -> Map k a
unions = foldl (flip plusFM) emptyFM

mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey = mapFM

elems :: Map k a -> [a]
elems = eltsFM

fromList :: Ord k => [(k,a)] -> Map k a
fromList = listToFM

fromListWith :: Ord k => (a -> a -> a) -> [(k,a)] -> Map k a 
fromListWith c = addListToFM_C (flip c) emptyFM

toAscList :: Map k a -> [(k,a)]
toAscList = fmToList
#endif
