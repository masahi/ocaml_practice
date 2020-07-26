(* Common operations used throughout the code *)
(* $Id$ *)


(* Direction, for loops *)
type dir = UP | DOWN
type perm = RowSwap of (int * int) | ColSwap of (int*int)
type 'a container2dfromvector = {arr:('a array); n:int; m:int}
type 'a container2dfromFvector = FortranVector of ('a array * int * int)
