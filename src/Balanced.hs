{------------------------------------------------------------------------------
			      SIZE BALANCED TREES

This module provides a Haskell version of the excellent SML size-balanced-tree
package presented bt Stephen Adams in the Fuctional Pearls section of the
October '93 edition of JFP [3(4):553-561].  [He and the literature call them
`bounded balance binary trees' but I prefer `size balanced (binary) tree' as it
is less of a mouthful.]  Being in the functional pearls section, the analysis
of the algorithm was brief so, until I can get hold of the background paper
(currently a Southampton ECS internal report), the commentary here is going to
be sketchy (why duplicate work, especially when it is this good?).

The main advantages of this package are as follows:

	* all operations are guaranteed to generate balanced trees

	* deletion is properly supported and `asymptopically logarithmic'

	* the `tree -> tree -> tree' union operation are suported and takes a
	  constant time in the bset case, degrading to a linear time in the
	  worst case (see the JFP article and the internal report for details).
	  In addition, intersection and difference operations are supported and
	  seem to be nearly as efficient (these operations are analysed in the
	  internal report).

	* the subset operation is supported nad ought to be at least as
	  efficient as union.

The disadvantage is, of course, that the basic insertion operation is quite
expensive compared to other aproaches; it is somewhat more expensive than a
straightforward implementation of red-black or 2-3-4 trees.  Some benchmarks
indicated that it was 2.5 times slower at inserting than a 2-3-4 package and
nearly 4 times slower than a straightforward binary search tree.  In an effort
to address this problem the `mk_balanced' function has been provided for
constructing a balanced tree monolithically from a list of elements.  This can
deals predictably with multiple elements, earlier items in the list taking
presedence over later ones.  Constructing a balanced tree monolithically should
be at least as fast as any other technique for constructing a search structure
and somewhat faster than (functional) incremental aproaches.  This means that
algorithms that work by gathering the data into a search structure then
searching it ought to work well (as distinct form algorithms that need to
search the structure as it is built).

Adams' package used SML Functors to abstract the ordering function; the
standard Haskell/Gofer solution would be to use type classes to achieve the
same purpose, however, the `traditional' solution of passing a higher order
function is used here instead, thereby making it easier to port to programming
languages that do not support Haskell style type classes.

The `delete_b', `intersection_b' and `difference_b' operations were set as
problems for the reader in the JFP article so the versions given here are my
extrapolation of the techniques presented in the paper.  They will be checked
against the internal report as soon as I get a hold of it.

The main additions to the package presented by Adams is to include a function
for constructing a perfectly balanced tree from a list of elements, some extra
functions for folding, iterating over and mapping (`fold_b', `for_b' and
`map_b'), generalised version of `in' and `add' (`find_b' and `combine_b'; see
below), and a function for establishing whether one tree contains all the
elements in another (`subset_t').

Chris Dornan, 16-May-94
------------------------------------------------------------------------------}

module Balanced(
	Balanced(EmptyB,NodeB),
	size_b,empty_b,singleton_b,
	mk_balanced,mk_balanced',build_b,
	is_empty_b,fold_b,foldr_b,for_b,map_b,
	in_b,find_b,find_n_b,add_b,combine_b,delete_b,del_min_b,
	split_b,union_b,difference_b,intersection_b,subset_b,union_b'
	) where

import Sort


infix  4 %<<


is_full :: Maybe a -> Bool
is_full Nothing = False
is_full (Just _) = True



{------------------------------------------------------------------------------
			     Balanced Binary Trees
------------------------------------------------------------------------------}



-- The balanced binary trees presented here differ from ordinary binary search
-- trees in the number of elements contained in a tree is stored with each
-- node.  This size information is then used to balance the tree.  The number
-- of elements in a balanced tree is almost as good as its height for
-- balancing, the height being proportional to the log of the number of
-- elements in the tree.  The size of the tree is easier to maintain and the
-- information is more useful (for determining the cardinality of a set, etc.)
-- According to the JFP article, such size balanced trees can be searched with
-- only a few percent efficiency loss over true height balanced trees.


data Balanced a = EmptyB | NodeB a Int (Balanced a) (Balanced a)


-- The algorithms presented here are structured around a collection of
-- `increasingly inteligent' node constructors that take an element and two
-- trees, such that all the elements in the first tree is strictly less than
-- the element and all the elements in the second tree are strictly greater,
-- and and combine them to produce a single tree.  All these constructors will
-- be of type `BCons a' (or possibly `(a->a->Bool) -> BCons a', where the first
-- argument is the ordering function).  The first one `nodeB' constructs a
-- tree, maintaining the size invariant.

type BCons a = a -> Balanced a -> Balanced a -> Balanced a

nodeB:: BCons a
nodeB x l r = NodeB x (1+size_b l+size_b r) l r

size_b:: Balanced a -> Int
size_b EmptyB = 0
size_b (NodeB _ n _ _) = n


-- This operator determines whether the size of the tree represented by its
-- left argument is sufficiently smaller than the tree in its right argument to
-- warrant rebalancing if the two trees were combined with `nodeB'.  The two
-- sizes are kept within a constant factor so that their heights, which are
-- proportional to the logarithms of the sizes, will remain the same within a
-- given tolerance (which is approximately log_2 5).  This constant can be used
-- to tune the tradeoff in the effort given to balancing against the
-- performance loss of searching poorly balanced trees.  (Much effort is paid
-- up front in maintaining the size of trees anyway so it probably makes little
-- sense to use a large constant.)
 
(%<<):: Int -> Int -> Bool
n %<< n' = n*5<n'


empty_b:: Balanced a
empty_b = EmptyB

singleton_b:: a -> Balanced a
singleton_b x = NodeB x 1 EmptyB EmptyB


-- This function constructs a balanced tree from a list of elements.  The
-- function in its first argument should (totally) order the elements.  It
-- completes in time proportional to `n log(n)' where `n' is the number of
-- elements in the list.  "mk_balanced'" performs the same function but expects
-- to be passed an ordered list.

mk_balanced:: (a->a->Bool) -> [a] -> Balanced a
mk_balanced (<) xs = mk_balanced' (nub' le xs)
      where
	le x y = not(y<x)

mk_balanced':: [a] -> Balanced a
mk_balanced' xs = build_b (length xs) xs const

build_b:: Int -> [a] -> (Balanced a->[a]->b) -> b
build_b 0 xs f = f EmptyB xs
build_b n' xs f | n>=0 =
	build_b hn xs 					$ \l ~(x:xs) ->
	build_b (n-hn) xs 				$ \r xs ->
	f (NodeB x n' l r) xs
	where
	hn =  n `div` 2
	n = n'-1
build_b _ _ _ = error "build_b"


-- `is_empty_b' tests for emptiness; `fold_b' is the `Hughes fold operator' for
-- binary trees; `foldr_b f k t' is equivelent to `foldr f k l' where `l' is
-- the list produced by in-order traversal of `t'; `for_b' iterates over trees
-- and `map_b' maps them.

is_empty_b:: Balanced a -> Bool
is_empty_b EmptyB = True
is_empty_b (NodeB _ _ _ _) = False

fold_b:: (a->Int->b->b->b) -> b -> Balanced a -> b
fold_b nd em EmptyB = em
fold_b nd em (NodeB x c l r) = nd x c (fold_b nd em l) (fold_b nd em r)


-- although neat, this is probably more expensive than the direct version

foldr_b:: (a->b->b) -> b -> Balanced a -> b
foldr_b f k t = fold_b (\x _ l r->l.f x.r) id t k

for_b:: (s->a->(s,b)) -> s -> Balanced a -> (s,Balanced b)
for_b f s0 EmptyB = (s0,EmptyB)
for_b f s0 (NodeB x c l r) = (s3,NodeB x' c l' r')
	where
	(s1,l') = for_b f s0 l
	(s2,x') = f        s1 x
	(s3,r') = for_b f s2 r

map_b:: (a->b) -> Balanced a -> Balanced b
map_b f = fold_b (\x c l r->NodeB (f x) c l r) EmptyB 




{------------------------------------------------------------------------------
			    Inserting and Deleting
------------------------------------------------------------------------------}



-- These smart node-constructor functions rotate the top of a tree and are used
-- by the next smart constructor, `balB' that constructs a balanced binary tree
-- from two balanced trees of nearly the same size; i.e., they must have been
-- unbalanced by the insertion or deletion of at most one element (see the JFP
-- article/internal report for details).

single_lB,double_lB,single_rB,double_rB:: BCons a

single_lB a x (NodeB b _ y z) = nodeB b (nodeB a x y) z
single_lB _ _ _ = error "single_lB"

double_lB a x (NodeB c _ (NodeB b _ y1 y2) z) =
					nodeB b (nodeB a x y1) (nodeB c y2 z)
double_lB _ _ _ = error "double_lB"

single_rB b (NodeB a _ x y) z = nodeB a x (nodeB b y z)
single_rB _ _ _ = error "single_rB"

double_rB c (NodeB a _ x (NodeB b _ y1 y2)) z =
					nodeB b (nodeB a x y1) (nodeB c y2 z)
double_rB _ _ _ = error "double_rB"

balB:: BCons a
balB x l r =
	if ln+rn < 2
	   then nodeB x l r
	   else if ln%<<rn
		   then let NodeB _ _ rl rr = r
			    rln = size_b rl
			    rrn = size_b rr
			in
			if rln<rrn
			   then single_lB x l r
			   else double_lB x l r
		   else if rn%<<ln
			   then let NodeB _ _ ll lr = l
				    lln = size_b ll
				    lrn = size_b lr
				in
				if lrn<lln
				   then single_rB x l r
				   else double_rB x l r
			   else nodeB x l r
	where
	ln = size_b l
	rn = size_b r


-- `in_b' tests for membership, `add_b' adds an element to a balanced tree,
-- maintaining its balance and likwise `delete_b' deletes an element form a
-- binary tree without upsetting its balance it.  `find_n_b' finds the n_th
-- element (starting at 0) in logarithmic time.  `find_b' and `combine_b' are
-- generalisations of these functions: `find_b' returns the element if it is
-- present and `combine_b' uses a function to adjust an element in the tree (it
-- combines information about an element that may be already in the tree with
-- some new information); the function passed to `combine_b' will be passed
-- nothing if the element was not in the tree, otherwise it will be passed the
-- existing element.  These generalised functions are intended to be used in
-- with trees that contain elements for which only part of them is used to
-- order the tree: the `key' components.
--  
-- Note the way `combine_b' and `delete_b' are straightforward implemetations,
-- with the same structure as the same functions for vanilla binary search
-- trees, except that they use `balB' rather than `NodeB' or `nodeB' to
-- construct nodes.

in_b:: (a->a->Bool) -> a -> Balanced a -> Bool
in_b (<) x t = is_full(find_b (<) x t)

find_b:: (a->a->Bool) -> a -> Balanced a -> Maybe a
find_b (<) x EmptyB = Nothing
find_b (<) x (NodeB x' _ l r) =
	if x<x'
	   then find_b (<) x l
	   else if x'<x
		   then find_b (<) x r
		   else Just x'

find_n_b:: Int -> Balanced a -> a
find_n_b n EmptyB = error "Balanced:find_n_b"
find_n_b n (NodeB x _ l r) =
	if n<ln
	   then find_n_b n l
	   else if ln<n
		   then find_n_b (n-ln-1) r
		   else x
	where
	ln = size_b l

add_b:: (a->a->Bool) -> a -> Balanced a -> Balanced a
add_b (<) x t = combine_b (<) x (const x) t

combine_b:: (a->a->Bool) -> a -> (Maybe a->a) -> Balanced a -> Balanced a
combine_b (<) x f EmptyB = singleton_b (f Nothing)
combine_b (<) x f st@(NodeB v c l r) =
	if x<v
	   then balB v (combine_b (<) x f l) r
	   else if v<x
		   then balB v l (combine_b (<) x f r)
		   else NodeB (f(Just v)) c l r

delete_b:: (a->a->Bool) -> a -> Balanced a -> Balanced a
delete_b (<) x EmptyB = EmptyB
delete_b (<) x (NodeB v n l r) =
	if x<v
	   then balB v (delete_b (<) x l) r
	   else if v<x
		   then balB v l (delete_b (<) x r)
		   else case r of
			  EmptyB -> l
			  _ ->	del_min_b r (\v' r'->balB v' l r')

del_min_b:: Balanced a -> (a->Balanced a->c) -> c
del_min_b (NodeB v n l r) f =
	case l of
	  EmptyB -> f v r
	  _ ->	del_min_b l (\x l'->f x (balB v l' r))
del_min_b _ _ = error "del_min_b"



{------------------------------------------------------------------------------
				Combining Trees
------------------------------------------------------------------------------}



-- This is the last in line of smart constructors.  `concatB' constructs a
-- balanced binnary tree, like `balB', except that the (balanced) trees in its
-- argumment may be of arbitrary sizes relative to each other.  It completes in
-- log(n) time, where n is the number of elements in the trees passed to it.

concatB:: (a->a->Bool) -> BCons a
concatB (<) v EmptyB r = add_b (<) v r
concatB (<) v l@(NodeB _ _ _ _) EmptyB = add_b (<) v l
concatB (<) v l@(NodeB lv ln ll lr) r@(NodeB rv rn rl rr) =
	if ln%<<rn
	   then balB rv (concatB (<) v l rl) rr
	   else if rn%<<ln
		   then balB lv ll (concatB (<) v lr r)
		   else nodeB v l r


-- The `split_b' function does the oposite of `concatB': it partitions its
-- argument tree into two balanced trees such that all the elements lees than a
-- given element are present in one of the trees and all the elements greater
-- are in another: if the pivot element itself is present then this is returned
-- too.  The multiple outputs to this function are handled by taking a
-- continuation function that is fed the results of the partition.

split_b ::(a->a->Bool) -> a -> Balanced a ->
				(Maybe a->Balanced a->Balanced a->b) -> b
split_b (<) x EmptyB f = f Nothing EmptyB EmptyB
split_b (<) x (NodeB v _ l r) f =
	if x<v
	   then split_b (<) x l					$ \mv' l' r' ->
		f mv' l' (concatB (<) v r' r)
	   else if v<x
		   then split_b (<) x r				$ \mv' l' r' ->
			f mv' (concatB (<) v l l') r'
		   else f (Just v) l r


-- The `union_b', `difference_b' and `intersection_b' functions combine sets to
-- provide their `set-theoretic' union, intersection and difference using a
-- divide and conquor approach.  They all use the same aproach: split one of
-- the trees into two equal sized trees at the root, using the element in the
-- root to partition the other argument tree using split.  This partitions both
-- the inputs around a common element; the function is recursively called to
-- solve the problem for each of the partitions with provides two trees, one
-- with all the elements less than the pivot element and another with all the
-- elements greater then the pivot, which are then combined with the pivot
-- element using `concat'; finally, it may be necessary to remove the pivot
-- element from the result, depending upon whether it was actually present in
-- the set which was partionend with `split_b'.  To take `difference_b' for
-- example, the set in the left argument is split naturally and the one in its
-- right argument is partitioned with `split_b'; the partitions are
-- individually subtracted and the results are combined with the pivot element
-- using `concatB'; if the pivot element was not in the right argument then
-- this is the result otherwise the pivot element has to be removed from the
-- result with `delete_b'.  The same technique was used with `union_b' (where
-- the pivot is never deleted) and `intersection_b' (where the pivot is deleted
-- if it does not appear in both sets).
--  
-- `subset_b' returns true if the all the elements in its second argument
-- appear in its third argument.  It is even simpler as it does not need to
-- combine the partitions.

union_b:: (a->a->Bool) -> Balanced a -> Balanced a -> Balanced a
union_b (<) EmptyB t = t
union_b (<) t@(NodeB _ _ _ _) EmptyB = t
union_b (<) t@(NodeB _ _ _ _) (NodeB a' _ l' r') =
	split_b (<) a' t 					$ \_ l r ->
	concatB (<) a' (union_b (<) l l') (union_b (<) r r')


-- *** Should we consider introducing a `graft_b'? ***

difference_b:: (a->a->Bool) -> Balanced a -> Balanced a -> Balanced a
difference_b (<) EmptyB t = EmptyB
difference_b (<) t@(NodeB _ _ _ _) EmptyB = t
difference_b (<) (NodeB a _ l r) t@(NodeB _ _ _ _) =
	split_b (<) a t						$ \ma' l' r' ->
	let t_i = concatB (<) a (difference_b (<) l l')
				 (difference_b (<) r r')
	in
	if is_full ma' then delete_b (<) a t_i else t_i
difference_b _ _ _ = error "difference_b"

intersection_b:: (a->a->Bool) -> Balanced a -> Balanced a -> Balanced a
intersection_b (<) EmptyB _ = EmptyB
intersection_b (<) (NodeB _ _ _ _) EmptyB = EmptyB
intersection_b (<) t@(NodeB _ _ _ _) (NodeB a' _ l' r') =
	split_b (<) a' t					$ \mb_a l r ->
	let t_i = concatB (<) a' (intersection_b (<) l l')
				  (intersection_b (<) r r')
	in
	if is_full mb_a then t_i else delete_b (<) a' t_i

subset_b:: (a->a->Bool) -> Balanced a -> Balanced a -> Bool
subset_b (<) EmptyB _ = True
subset_b (<) (NodeB _ _ _ _) EmptyB = False
subset_b (<) (NodeB a _ l r) t@(NodeB _ _ _ _) =
	split_b (<) a t						$ \ma' l' r' ->
	if is_full ma'
	   then subset_b (<) l l' && subset_b (<) r r'
	   else False

-- This is an alternative version of `union_b' that takes an extra function to
-- control the integration of the elements in the right hand argument into the
-- set in the left argument: the right argument to `f' will be the element to
-- be integrated from the right set and the left hand argument will be the
-- corresponding element in the left hand set if there is one, `Nothing'
-- otherwise.  `union_b' could have been expressed as:
--	 
--	union_b (<) = union_b' (<) const
--  
-- but this was not done to avoid having to process every element of the right
-- hand set.

union_b':: (a->a->Bool) -> (Maybe a->a->a) ->
				Balanced a -> Balanced a -> Balanced a
union_b' (<) f EmptyB t = t
union_b' (<) f t@(NodeB _ _ _ _) EmptyB = map_b (f Nothing) t
union_b' (<) f t@(NodeB _ _ _ _) (NodeB a' _ l' r') =
	split_b (<) a' t 					$ \mb_a l r ->
	concatB (<) (f mb_a a') (union_b' (<) f l l')
				(union_b' (<) f r r')

