{------------------------------------------------------------------------------
				      DFS

This module is a portable version of the ghc-specific `DFS.g.hs', which is
itself a straightforward encoding of the Launchbury/King paper on linear graph
algorithms.  This module uses balanced binary trees instead of mutable arrays
to implement the depth-first search so the complexity of the algorithms is
n.log(n) instead of linear.

The vertices of the graphs manipulated by these modules are labelled with the
integers from 0 to n-1 where n is the number of vertices in the graph.

The module's principle products are `mk_graph' for constructing a graph from an
edge list, `t_close' for taking the transitive closure of a graph and `scc'
for generating a list of strongly connected components; the components are
listed in dependency order and each component takes the form of a `dfs tree'
(see Launchberry and King).  Thus if each edge (fid,fid') encodes the fact that
function `fid' references function `fid'' in a program then `scc' performs a
dependency analysis.

Chris Dornan, 23-Jun-94, 2-Jul-96, 29-Aug-96, 29-Sep-97
------------------------------------------------------------------------------}

module DFS where

import Array
import Map


-- The result of a depth-first search of a graph is a list of trees,
-- `GForrest'.  `post_order' provides a post-order traversal of a forrest.

type GForrest = [GTree]
data GTree    = GNode Int GForrest

postorder:: GForrest -> [Int]
postorder ts = po ts []
	where
	po ts l = foldr po_tree l ts

	po_tree (GNode a ts) l = po ts (a:l)

list_tree:: GTree -> [Int]
list_tree t = l_t t []
	where
	l_t (GNode x ts) l = foldr l_t (x:l) ts


-- Graphs are represented by a pair of an integer, giving the number of nodes
-- in the graph, and function mapping each vertex (0..n-1, n=size of graph) to
-- its neighbouring nodes.  `mk_graph' takes a size and an edge list and
-- constructs a graph.

type Graph = (Int,Int->[Int])
type Edge = (Int,Int)

mk_graph:: Int -> [Edge] -> Graph
mk_graph sz es = (sz,\v->ar!v)
	where
	ar = accumArray (flip (:)) [] (0,sz-1) [(v,v')| (v,v')<-es]

vertices:: Graph -> [Int]
vertices (sz,_) = [0..sz-1]

out:: Graph -> Int -> [Int]
out (_,f) = f

edges:: Graph -> [Edge]
edges g = [(v,v')| v<-vertices g, v'<-out g v]

rev_edges:: Graph -> [Edge]
rev_edges g = [(v',v)| v<-vertices g, v'<-out g v]

reverse_graph:: Graph -> Graph
reverse_graph g@(sz,_) = mk_graph sz (rev_edges g)


-- `t_close' takes the transitive closure of a graph; `scc' returns the stronly
-- connected components of the graph and `top_sort' topologically sorts the
-- graph.  Note that the array is given one more element in order to avoid
-- problems with empty arrays.

t_close:: Graph -> Graph
t_close g@(sz,_) = (sz,\v->ar!v)
	where
	ar = listArray (0,sz) ([postorder(dff' [v] g)| v<-vertices g]++[und])
	und = error "t_close"

scc:: Graph -> GForrest
scc g = dff' (reverse (top_sort (reverse_graph g))) g

top_sort:: Graph -> [Int]
top_sort = postorder . dff 


-- `dff' computes the depth-first forrest.  It works by unrolling the
-- potentially infinite tree from each of the vertices with `generate_g' and
-- then pruning out the duplicates.

dff:: Graph -> GForrest
dff g = dff' (vertices g) g

dff':: [Int] -> Graph -> GForrest
dff' vs (bs,f) = prune (map (generate_g f) vs)

generate_g:: (Int->[Int]) -> Int -> GTree
generate_g f v = GNode v (map (generate_g f) (f v))

prune:: GForrest -> GForrest
prune ts = snd(chop(empty_int,ts))
	where
	empty_int:: MSet Int
	empty_int = empty (<)

chop:: (MSet Int,GForrest) -> (MSet Int,GForrest)
chop p@(vstd,[]) = p
chop (vstd,GNode v ts:us) =
	if v `elt` vstd
	   then chop (vstd,us)
	   else let vstd1 = ins v () vstd
		    (vstd2,ts') = chop (vstd1,ts)
		    (vstd3,us') = chop (vstd2,us)
		in
		(vstd3,GNode v ts' : us')


{-- Some simple test functions

test:: Graph Char
test = mk_graph (char_bds ('a','h')) (mk_pairs "eefggfgegdhfhged")
	where
	mk_pairs [] = []
	mk_pairs (a:b:l) = (a,b):mk_pairs l

-}
