{-# LANGUAGE CPP #-}
module Set ( Set, member, empty, insert ) where

import Data.Set 

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 603
member :: Ord a => a -> Set a -> Bool
member = elementOf

empty  :: Set a
empty = emptySet

insert :: Ord a => a -> Set a -> Set a
insert = flip addToSet
#endif
