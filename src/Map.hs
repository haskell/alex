{------------------------------------------------------------------------------
			      LOOKUP TABLES

This module provides a lookup-table facility along the lines of the
set-theoretic functions of Z and maps of VDM (hence the name).  Thus, a map is
conceptually a set of associations, associating elements of the domain with
elements of the range.  To support efficient storage of these associations, a
total ordering relation on the domain (<) must be provided when the map is
created.

A bounded balanced tree package is used to store the associations so all the
single-element lookup and storage operations are logarithmic in the worst case
and the map the combining functions are linear in the worst case.  Constructing
a map from a list of `n' associations is `n log(n)' in the worst case.  In many
instances, importance special cases are dealt with more efficiently: see the
`Balance' package for more details.

Incremental versus Monolithic Map Construction
----------------------------------------------

An algorithm can be classified according to how it uses a Map: it could collect
all the associations into a map and then proceed to search the map without
updating it again; alternatively it could completely interleave all its
additions and searches.  Of course it could be somewhere in between these
extremes, but if it is of the former type then it should collect its additions
into an association list and build the map thus:

	empty (<) <+> al

rather than

	foldr (\(x,y)->ins x y) (empty (<)) al

as the first is much more efficient for large lists because it will use a fast
sorting method to order the elements (completing in linear time if the elements
are already ordered) and then construct a balanced tree.  They are both have
the same worst case algorithmic complexity (n.log(n)) but building a structure
monolithically is bound to be more efficient than trying to keep it balanced as
the same elements are added to it incrementally.

Combining Maps
--------------

When the map is created, the ordering relation to be used when manipulating the
map is passed to the `empty' function and this relation is stored in the map
along with the associations.  This scheme works fine for `Map' functions that
take only one map in their argument but creates a problem for functions that
combine two maps: which ordering relation should be used?  The answer is that
if the algorithms rely on both maps being ordered with the same relation, so
the function should be invoked with maps conaining the same ordering relation.
The relation in the left hand argument is used.

Sets
----

Every map may be considered to be representing a set: its domain.  Thus `MSet
a' is type synonym for `Map a ()'.

Chris Dornan, 17-May-94, 19-Jul-94, 10-Oct-94
------------------------------------------------------------------------------}

module Map
       (Map,map_lt,empty,is_empty,card,members,dom,rng,elt,app,app',app_n,
	oplus,oplus',combine,ins,del,(<+>),(<%>),(<\>),
	map_map,map_map',foldr_map,for_map,filter_map,
	MSet,cup,cap,less,subset,psubset,(<++>)) where

import Maybe
import Sort
import Balanced


-- A map consists of the ordering relation (<) and a balanced tree of
-- associations.

data Map a b = Mp (a->a->Bool) (Balanced (a,b))


-- While the ordering relation passed to this package orders the domain (a),
-- the tree package works with an ordering on the elements stored in the tree
-- (Assoc a b); the `cnv' function converts the ordering function passed to
-- this package into the ordering relation that is required for `Balance': the
-- left association is less than the right association iff the domain elements
-- are so ordered according to the domain ordering function.
--  
-- A similar problem occurs when looking up elements in the tree that
-- correspond to a given domain element: the tree package expects item of type
-- `Assoc a b' whereas we only have some thing of type `a'; the solution
-- adopted by "cnv'" is to pass `(x,_L)'.

cnv:: (a->a->Bool) -> (a,b) -> (a,b) -> Bool
cnv op (x,_) (x',_) = op x x'

cnv':: a -> (a,b)
cnv' x = (x,error "Map:cnv'")


-- These are all fairly standard table lookup operations.  Note that `elt' is
-- realy named as a set operation as it tests whether its first arguments is in
-- the domain of the map in its second argument; "app'" returns Nothing if its
-- first argument is not in the domain of the map; "oplus" merges two maps,
-- allowing the associations in the right hand argument to take precedence; it
-- gets its name from the name given to the `(+)' operator in TeX, which is
-- used to represent function overriding in Z; `combine is a generalisation of
-- `ins' that takes a function which is passed the element of the map that it
-- is to displace and `Nothing' if there is none.
--  
-- "oplus'" is a special version of `oplus' that allows the integration of the
-- elements from the right map into the left map to be controlled.  The
-- function passed to it will have the element to be integrated from the right
-- set passed in its right argument and the element that it is to displace from
-- the left set will be passed in its left argument if there is a corresponding
-- element in the left set, `Nothing' otherwise.  The standard `oplus' operator
-- could have been defined:
--	 
--	oplus = oplus' (\_ _ y->y)
--  
-- and a version that gives priority to elements in the left map could be
-- defined:
--	 
--	oplus'' = oplus' (\_ mb_y y -> result(mb_y$?Just y))

map_lt:: Map a b -> (a->a->Bool)
map_lt (Mp (<) _) = (<)

empty:: (a->a->Bool) -> Map a b
empty (<) = Mp (<) empty_b

is_empty:: Map a b -> Bool
is_empty (Mp _ tr) = is_empty_b tr

card:: Map a b -> Int
card (Mp _ t) = size_b t

members:: Map a b -> [(a,b)]
members (Mp _ t) = foldr_b (:) [] t

dom:: Map a b -> [a]
dom (Mp _ t) = foldr_b (\(x,_) t->x:t) [] t

rng:: Map a b -> [b]
rng (Mp _ t) = foldr_b (\(_,y) t->y:t) [] t

elt:: a -> Map a b -> Bool
elt x mp = isJust(app' mp x)

app:: Map a b -> a -> b
app mp = fromJust . app' mp

app':: Map a b -> a -> Maybe b
app' (Mp lt t) x = fmap snd (find_b (cnv lt) (cnv' x) t)

app_n:: Map a b -> Int -> (a,b)
app_n (Mp _ t) n = find_n_b n t

oplus:: Map a b -> Map a b -> Map a b
oplus (Mp lt tr) (Mp _ tr') = Mp lt (union_b (cnv lt) tr tr')

oplus':: (a->Maybe b ->b->b) -> Map a b -> Map a b -> Map a b
oplus' f (Mp lt tr) (Mp _ tr') = Mp lt (union_b' (cnv lt) f' tr tr')
	where
	f' mb_xy (x,y) = (x,f x (fmap snd mb_xy) y)

combine:: a -> (Maybe b->b) -> Map a b -> Map a b
combine x f (Mp lt tr) = Mp lt (combine_b (cnv lt) (cnv' x) f' tr)
	where
	f' mb_a = (x,f(fmap snd mb_a))

ins:: a -> b -> Map a b -> Map a b
ins x y = combine x (const y)

del:: a -> Map a b -> Map a b
del x (Mp lt tr) = Mp lt (delete_b (cnv lt) (cnv' x) tr)


-- These operators add and subtract associations from a map.  Note that
-- associations appearing earlier in the association list take precedence,
-- i.e., the following equivelece holds:
--	 
--	empty (<) <+> al  <=>   foldr (\(x,y)->ins x y) (empty (<)) al
--  
-- Both `<+>' and `<\>' complete in time proportional to `n log(n+m)' where n
-- is the number of elements in the list and `m' the number of elements in the
-- map.  If the elements in the domain of the association list appear in
-- strictly increasing order then it will complete in time proportional to
-- `n+m'.  Note that as the association list becomes larger, it will be
-- inreasingly quicker to use these operators rather than incrementally adding
-- or removing them from the map.
--  
-- The `<%>' operator is a special version of `<+>' that checks to see whether
-- any of the associations in its right argument are already in the map or
-- duplicated within the list.  If one of none of the association are
-- duplicated then it will return the extended map, otherwise it returns one of
-- the offending duplicate associations.  It has the same complexity as <+>
-- (linear on ordered lists with no duplicates, n log(n) otherwise).

(<+>):: Map a b -> [(a,b)] -> Map a b
(<+>) (Mp lt t) l = 
	if not(is_empty_b t) && length l<10
	   then Mp lt (foldr (add_b lt') t l)
	   else Mp lt (union_b lt' t (mk_balanced lt' l))
	where
	lt' = cnv lt

(<%>):: Map a b -> [(a,b)] -> Either (a,b) (Map a b)
(<%>) (Mp lt t) l = 
	case f l' of
	   [] -> case intersection_b lt' t t' of
		   EmptyB -> Right(Mp lt (union_b lt' t t'))
		   NodeB x _ _ _ -> Left x
	   h:_ -> Left h
	where
	f [] = []
	f l@[_] = []
	f l@(h:t@(h':_)) = if lt' h h' then f t else l

	t' = mk_balanced' l'
	
	l' = msort lt' l

	lt' = cnv lt

(<\>):: Map a b -> [a] -> Map a b
(<\>) (Mp lt t) xs =
	if length xs<10
	   then Mp lt (foldl del t xs)
	   else Mp lt (difference_b lt' t (mk_balanced lt' (map cnv' xs)))
	where
	del t' x = delete_b lt' (cnv' x) t'

	lt' = cnv lt


-- These functions provide some of the standard list processing functions for
-- maps.  They complete in time proportional to the size of the map (this is
-- even true of `filter_map' as the list appearing in the right argument of
-- `<\>' in its body is ordered).  `foldr_map' is characterised by
--	 
--	foldr_map f k mp = foldr f k (members mp)
--  
-- `for_map' takes a state transformer, a start state and a map; it applies the
-- state transformer successively to each association in the map, strting with
-- the minimum element and finishing with the maximum element, and returns the
-- final state and the new map.
--  
-- "map_map'" is an operation that should be used with some care as the order
-- of the elements in the new map is not changed, regardless of the ordering
-- operator provided, therefore the mapping function, ordering on the elements
-- in the original map and the new ordering relation must agree in the
-- following sense:
--	 
--	cnv le a a' = cnv le' (f a) (f a')
--  
-- where "a" and "a'" are arbitrary associations in the original map, "le" is
-- the original ordering relation, "le'" is the new ordering relation and "cnv"
-- is the function at the beginning of this module for converting domain
-- ordering relations to those operation on associations.  A safer but less
-- efficient way of achieveing the same reult is:
--  
--	empty le' <+> map f (members mp)
--  
-- If the above equivelece holds then the list on the right of the `<+>' will
-- be ordered and it will in fact complete in linear time, but, on large lists,
-- will still take longer and will consume more heap.

map_map:: (a->b->c) -> Map a b -> Map a c
map_map f mp@(Mp lt _) = map_map' (\(x,y)->(x,f x y)) lt mp

map_map':: ((a,b)->(c,d)) -> (c->c->Bool) -> Map a b -> Map c d
map_map' f lt (Mp _ tr) = Mp lt (map_b f tr)

foldr_map:: ((a,b)->c->c) -> c -> Map a b -> c
foldr_map f k (Mp _ t) = foldr_b f k t

for_map:: (s->a->b->(s,c)) -> s -> Map a b -> (s,Map a c)
for_map f s (Mp lt tr) = (s',Mp lt tr')
	where
	(s',tr') = for_b f' s tr

	f' s (x,y) = case f s x y of {(s',y')->(s',(x,y'));}

filter_map:: (a->b->Bool) -> Map a b -> Map a b
filter_map p mp = mp <\> [x|(x,y)<- members mp, not(p x y)]



-- Here are the special set operations.  Like `oplus' above, some of their
-- names have been derived from the names given to TeX symbols that represent
-- the operations.  They all complete in time proportional to the number of
-- elements in both sets, except `<++>' which inherits the algorithmic
-- properties of `<+>'.  In some special cases, such as when one of the
-- arguments is an empty set, they will complete in constant time (see the
-- `Balance' module for more details).

type MSet a = Map a ()


cup:: Map a b -> Map a b -> Map a b
cup = oplus

cap:: Map a b -> Map a b -> Map a b
cap (Mp lt tr) (Mp _ tr') = Mp lt (intersection_b (cnv lt) tr tr')

less:: Map a b -> Map a b -> Map a b
less (Mp lt tr) (Mp _ tr') = Mp lt (difference_b (cnv lt) tr tr')

subset:: Map a b -> Map a b -> Bool
subset (Mp lt tr) (Mp _ tr') = subset_b (cnv lt) tr tr'

psubset:: Map a b -> Map a b -> Bool
psubset st st' = card st /= card st' && st `subset` st'

(<++>):: MSet a -> [a] -> MSet a
(<++>) mp l = mp <+> zip l (repeat ())
