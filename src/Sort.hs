{------------------------------------------------------------------------------
                                 SORTING LISTS

This module provides a properly parameterised merge sort function, complete
with associated functions. It is based on a Bob Buckley's (Bob Buckley
18-AUG-95) coding of Knuth's natural merge sort (see Vol. 2).  It seems to be
fast in the average case; it makes use of natural runs in the data becomming
linear on ordered data; and it completes in worst time O(n.log(n)).  It is
divinely elegant.

`nub'' is an n.log(n) version of `nub' and `group_sort' sorts a list into
strictly ascending order, using a combining function in its arguments to
amalgamate duplicates.

Chris Dornan, 14-Aug-93, 17-Nov-94, 29-Dec-95
------------------------------------------------------------------------------}

module Sort where

-- Hide (<=) so that we don't get name shadowing warnings for it
import Prelude hiding ((<=))

msort :: (a->a->Bool) -> [a] -> [a]
msort _    [] = []                    -- (foldb f []) is undefined
msort (<=) xs = foldb (mrg (<=)) (runs (<=) xs)

runs :: (a->a->Bool) -> [a] -> [[a]]
runs (<=) xs0 = foldr op [] xs0
      where
        op z xss@(xs@(x:_):xss') | z<=x      = (z:xs):xss'
                                 | otherwise = [z]:xss
        op z xss                             = [z]:xss

foldb :: (a->a->a) -> [a] -> a
foldb _ [x] = x
foldb f xs0 = foldb f (fold xs0)
      where
        fold (x1:x2:xs) = f x1 x2 : fold xs
        fold xs         = xs

mrg:: (a->a->Bool) -> [a] -> [a] -> [a]
mrg _    [] l = l
mrg _    l@(_:_) [] = l
mrg (<=) l1@(h1:t1) l2@(h2:t2) =
        if h1<=h2
           then h1:mrg (<=) t1 l2
           else h2:mrg (<=) l1 t2


nub':: (a->a->Bool) -> [a] -> [a]
nub' (<=) l = group_sort (<=) const l


group_sort:: (a->a->Bool) -> (a->[a]->b) -> [a] -> [b]
group_sort le cmb l = s_m (msort le l)
        where
        s_m [] = []
        s_m (h:t) = cmb h (takeWhile (`le` h) t):s_m (dropWhile (`le` h) t)
