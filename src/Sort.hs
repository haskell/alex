{------------------------------------------------------------------------------
				 SORTING LISTS

This module provides properly parameterised insertion and merge sort functions,
complete with associated functions for inserting and merging.  `isort' is the
standard lazy version and can be used to the minimum k elements of a list in
linear time.  The merge sort is based on a Bob Buckley's (Bob Buckley
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


-- `isort' is an insertion sort and is here for historical reasons; msort is
-- better in almost every situation.

isort:: (a->a->Bool) -> [a] -> [a]
isort (<=) = foldr (insrt (<=)) []

insrt:: (a->a->Bool) -> a -> [a] -> [a]
insrt (<=) e [] = [e]
insrt (<=) e l@(h:t) = if e<=h then e:l else h:insrt (<=) e t


msort :: (a->a->Bool) -> [a] -> [a]
msort (<=) [] = []                    -- (foldb f []) is undefined
msort (<=) xs = foldb (mrg (<=)) (runs (<=) xs)

runs :: (a->a->Bool) -> [a] -> [[a]]
runs (<=) xs = foldr op [] xs
      where
	op z xss@(xs@(x:_):xss') | z<=x      = (z:xs):xss'
                                 | otherwise = [z]:xss
	op z xss                             = [z]:xss

foldb :: (a->a->a) -> [a] -> a
foldb _ [x] = x
foldb f xs  = foldb f (fold xs)
      where
	fold (x1:x2:xs) = f x1 x2 : fold xs
	fold xs         = xs

mrg:: (a->a->Bool) -> [a] -> [a] -> [a]
mrg (<=) [] l = l
mrg (<=) l@(_:_) [] = l
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
