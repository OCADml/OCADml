(**
   Construction of 3d vector space partitioning
   {{:https://en.wikipedia.org/wiki/Ball_tree} ball tree} structures for
   {{:https://en.wikipedia.org/wiki/Nearest_neighbor_search} nearest neighbour
   search}.  Implementation adapted from the
   {{:https://github.com/revarbat/BOSL2/blob/master/vectors.scad#L483} BOSL2
   vectors module}.

   Construction of these trees should be O(n log n) and searches should be
   O(log n), though real life performance depends on how the data is
   distributed. Thus, this structure is primarily useful when performing many
   searches of the same data. Below a certain number of points, it may be better
   to perform a direct search. *)

include (BallTree.Make (V3) (BallTree.Proj3) : BallTree.S with type vec := V3.t)
