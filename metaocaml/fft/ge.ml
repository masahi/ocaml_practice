open StateCPSMonad
(* open Prelude *)

module Let_syntax = struct
  let (let*) d f = bind d f
end

open Let_syntax

type update_kind = FractionFree | DivisionBased

(* It used to be that we used an option type to encode some domain
   information, like the need to compute a Determinant, Rank or Pivot.
   Now this is done directly with an exception instead, so these routines
   are no longer needed.
   For more detail, see the message
http://caml.inria.fr/pub/ml-archives/caml-list/2006/11/df527bcc780e6f3106889e2d5e8b5e2a.en.html
  Posted on the caml-list on Nov 17, 2006.
let fromJust = function Some x -> x | None -> failwith "Can't happen"
let notNone v str = match v with None -> failwith str | Some _ -> () *)
let ensure cond str = if not cond then failwith str else ()


(* A few utility functions to handle `open records', lists of variants *)
(* Each `field' is characterized by a triple: injection, projection functions
   and the name. The latter is used for printing error messages.
*)
type ('a,'b) open_rec = ('a -> 'b) * ('b -> 'a option)  * string

let rec lookup ((_,prj,_) as ip:(('a,'b) open_rec) )
   : 'b list -> 'a =
   function [] -> raise Not_found
   | (h::t) -> (match prj h with Some x -> x | _ -> lookup ip t)

let orec_store ((inj,_,name) as ip:(('a,'b) open_rec)) (v:'a) (s:'b list)
   : 'b list =
  let () =
    try let _ = lookup ip s in
        failwith ("The field of an open record is already present: " ^ name)
    with Not_found -> () in
  (inj v)::s

let orec_find ((_,_,name) as ip:(('a,'b) open_rec)) (s:'b list) : 'a =
  try lookup ip s
  with Not_found -> failwith ("Failed to locate orec field: " ^ name)

let mo_extend (ip:('a,'b) open_rec) (v:'a) : ('c, unit) monad =
  let* s = fetch in
  store (orec_store ip v s)

let mo_lookup (ip:('a,'b) open_rec) : ('c, 'a) monad =
  let* s = fetch in
  ret (orec_find ip s)


(* A lot of "Linear Algebra" is generic, so structure things
   to leverage that.  Some modules are container-independent,
   so these are pulled out.
*)

module LAMake(CODE: Coderep.T) = struct

module D = Domains_sig.S(
  struct type ('a, 'v) rep = ('a,'v) CODE.abstract end)
open D
open CODE

(* Was needed for debugging of the kind problem. Please keep it, just in case.
module Foo(D:DOMAINL with type kind = domain_is_field) = (* debugging *)
struct
  module D = D
end
*)

(* Monad used in this module:
   (abstract) code generation monad with open union state *)
type ('pc,'p,'a) cmonad_constraint = unit
      constraint
	  'p = <state : 's list; answer : ('a,'w) abstract>
      constraint
          'pc = <classif : 'a; answer : 'w; state : 's; ..>

type ('pc,'v) cmonad = ('p,('a,'v) abstract) monad
      constraint _ = ('pc,'p,'a) cmonad_constraint

type ('pc,'v) omonad = ('p,('a,'v) abstract option) monad
      constraint _ = ('pc,'p,'a) cmonad_constraint

module Iters = struct
  let row_iter b c low high get body d =
    let newbody j =
        let* bjc = retN (get b j c) in
        body j bjc
    in  loopM low high newbody d
  let col_iter b r low high get body d =
    let newbody k =
        let* brk = ret (get b r k) in
        body k brk
    in  loopM low high newbody d
end


(* Here are the various design aspects of GE-type algorithms *)

(* Rank is container-independent *)

(* no need to make the type abstract here - just leads to other problems *)
(* Even if no rank is output, it needs to be tracked, as the rank
   is also the outer loop index! *)
(* Note how this module contains its own signature *)
module TrackRank =
  struct
  type 'a lstate = ('a, int ref) abstract
    (* some magic for the proper visibility of identifiers *)
  type 'a tag_lstate_ = [`TRan of 'a lstate ]
  type 'a tag_lstate = 'a tag_lstate_
  type ('pc,'v) lm = ('pc,'v) cmonad
    constraint 'pc = <state : [> 'a tag_lstate]; ..>

  let ip = (fun x -> `TRan x), (function `TRan x -> Some x | _ -> None),
           "TrackRank"
  let decl () =
      let* rdecl = retN (liftRef Idx.zero) in
      let* _ = mo_extend ip rdecl in
      ret rdecl
  let succ () =
   let* r = mo_lookup ip in
   assignM r (Idx.succ (liftGet r))

      (* The signature of the above *)
  module type RANK = sig
    type 'a tag_lstate = 'a tag_lstate_
    val decl   : unit -> ('b, int ref) lm
    val succ   : unit -> ('b, unit) lm
    val fin    : unit -> ('b, int) lm
  end
end

module Rank = struct
  include TrackRank
  let fin () =
      let* r = mo_lookup ip in
      ret (liftGet r)
end

module NoRank = struct
  include TrackRank
  let fin () = failwith "Rank is needed but is not computed"
end

module type PIVOTKIND = sig
  type perm_rep
  type 'a ira = ('a, int) abstract
  type 'a fra
  type 'a pra = ('a, perm_rep) abstract
  val add : 'a fra -> 'a pra -> 'a pra
  val empty : 'a ira -> 'a pra
  val rowrep : 'a ira -> 'a ira -> 'a fra
  val colrep : 'a ira -> 'a ira -> 'a fra
end

module PermList = struct
  type flip_rep = perm
  type perm_rep = perm list
  type 'a ira = ('a, int) abstract
  type 'a fra = ('a, flip_rep) abstract
  type 'a pra = ('a, perm_rep) abstract
  let add x l = CList.cons x l
  let empty _ = (CList.nil : 'a pra)
  let rowrep x y = liftRowSwap x y
  let colrep x y = liftColSwap x y
end

module RowVectorPerm = struct
  type flip_rep = int*int
  type perm_rep = int array
  type 'a ira = ('a, int) abstract
  type 'a fra = ('a, flip_rep) abstract
  type 'a pra = ('a, perm_rep) abstract
  let add x l = Array1Dim.setL l x
  let empty n = Array1Dim.init n
  let rowrep x y = Tuple.tup2 x y
  let colrep x y = Tuple.tup2 x y
end

module type TRACKPIVOT = sig
  type perm_rep
  type 'a ira = ('a, int) abstract
  type 'a fra
  type 'a pra
  type 'a lstate
  type 'pc pc_constraint = unit
    constraint 'pc = <state : [> `TPivot of 'a lstate ]; ..>
  type ('pc,'v) lm = ('pc,'v) cmonad
    constraint _  = 'pc pc_constraint
  type ('pc,'a) nm = ('p,unit) monad
    constraint _ = ('pc,'p,'a) cmonad_constraint
    constraint _ = 'pc pc_constraint
  val rowrep : 'a ira -> 'a ira -> 'a fra
  val colrep : 'a ira -> 'a ira -> 'a fra
  val decl : ('a, int) abstract -> ('pc,'a) nm
  val add : 'a fra ->
    (<state : [> `TPivot of 'a lstate ]; ..>, unit) omonad
  val fin : unit -> ('b,perm_rep) lm
end

module PivotCommon(PK:PIVOTKIND) =
  struct
  type perm_rep = PK.perm_rep
  type 'a ira = 'a PK.ira
  type 'a fra = 'a PK.fra
  type 'a pra = 'a PK.pra
  type 'a lstate = ('a, PK.perm_rep ref) abstract
  type 'pc pc_constraint = unit
    constraint 'pc = <state : [> `TPivot of 'a lstate ]; ..>
  type ('pc,'v) lm = ('pc,'v) cmonad
    constraint _  = 'pc pc_constraint
  type ('pc,'a) nm = ('p,unit) monad
    constraint _ = ('pc,'p,'a) cmonad_constraint
    constraint _ = 'pc pc_constraint
  let rowrep = PK.rowrep
  let colrep = PK.colrep
  let ip = (fun x -> `TPivot x), (function `TPivot x -> Some x | _ -> None),
           "Pivot"
end

module KeepPivot(PK:PIVOTKIND) = struct
  include PivotCommon(PK)
  let decl n =
      let* pdecl = retN (liftRef (PK.empty n)) in
      mo_extend ip pdecl
  let add v =
   let* p = mo_lookup ip in
   ret (Some (assign p (PK.add v (liftGet p))))
  let fin  () =
      let* p = mo_lookup ip in
      ret (liftGet p)
end

module DiscardPivot = struct
  include PivotCommon(PermList)
  let decl _ = ret ()
  let add _ = ret None
  let fin () = failwith "Pivot is needed but is not computed"
end

(* Given a container, we can generate a whole "Linear Algebra"
   set of modules and generators all based on that container.
   This layout makes sure the same container is shared (and thus
   the same domain at the same time).

   The main reason that some of the above modules are not contained
   in this:
   1) some of the are container-independent, so why put them in here?
*)

module GenLA(C:CONTAINER2D) = struct

(* Bundle up some information, helps abstract some argument lists *)
type 'a wmatrix = {matrix: 'a C.vc; numrow: ('a,int) abstract;
                   numcol: ('a,int) abstract}
type 'a curpos  = {rowpos: ('a, int) abstract; colpos: ('a, int) abstract}
type 'a curposval = {p: 'a curpos; curval: ('a, C.Dom.v) abstract}

module type DETERMINANT = sig
  type tdet = C.Dom.v ref
  type 'a lstate
  type 'pc pc_constraint = unit
    constraint 'pc = <state : [> `TDet of 'a lstate ];  ..>
  type ('pc,'v) lm = ('pc,'v) cmonad
    constraint _  = 'pc pc_constraint
  type ('pc,'v) om = ('pc,'v) omonad
    constraint _  = 'pc pc_constraint
  type 'pc nm = ('p,unit) monad
    constraint _ = ('pc,'p,_) cmonad_constraint
    constraint _ = 'pc pc_constraint
  val decl : unit -> 'b nm (* no code is generated *)
  val upd_sign  : unit -> ('b,unit) om
  val zero_sign : unit -> ('b,unit) lm
  val acc_magn  : ('a,C.Dom.v) abstract -> ('b,unit) lm
  val get_magn  : unit -> ('b,tdet) lm
  val set_magn  : ('a,C.Dom.v) abstract -> ('b,unit) lm
  val fin       : unit -> ('b,C.Dom.v) lm
end

module type LOWER = sig
  type 'a lstate = ('a, C.contr) abstract
  type ('pc,'v) lm = ('pc,'v) cmonad
    constraint 'pc = <state : [> `TLower of 'a lstate ];  ..>
  val decl   : ('a, C.contr) abstract -> ('b, C.contr) lm
  val updt   : 'a C.vc -> ('a,int) abstract -> ('a,int) abstract -> 'a C.vo ->
            'a C.Dom.vc -> ('b, unit) lm option
  val fin    : unit -> ('a,  C.contr) lm
  val wants_pack : bool
end

module type PIVOT =
      functor (D: DETERMINANT) ->
        functor (P: TRACKPIVOT) ->
          functor (L: LOWER) -> sig
 (* Find the pivot within [r,m-1] rows and [c,(n-1)] columns
    of containrer b.
    If pivot is found, permute the matrix rows and columns so that the pivot
    becomes the element (r,c) of the matrix,
    Return the value of the pivot option. Or zero?
    When we permute the rows of columns, we update the sign of the det.
 *)
 val findpivot : 'a wmatrix -> 'a curpos ->
   (<state : [> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ]; ..>,
    C.Dom.v option) cmonad
(* [> 'a D.tag_lstate | 'a P.tag_lstate] *)
end

module NoDet : DETERMINANT =
  struct
  type tdet = C.Dom.v ref
  type 'a lstate = unit
  let decl () = ret ()
  let upd_sign () = ret None
  let zero_sign () = unitL
  let acc_magn _ = unitL
  let get_magn () = ret (liftRef C.Dom.zeroL) (* hack alert! *)
  let set_magn _ = unitL
  let fin () = failwith "Determinant is needed but not computed"
  type 'pc pc_constraint = unit
    constraint 'pc = <state : [> `TDet of 'a lstate ]; ..>
  type ('pc,'v) lm = ('pc,'v) cmonad
    constraint _  = 'pc pc_constraint
  type ('pc,'v) om = ('pc,'v) omonad
    constraint _  = 'pc pc_constraint
  type 'pc nm = ('p,unit) monad
    constraint _ = ('pc,'p,_) cmonad_constraint
    constraint _ = 'pc pc_constraint
end

module AbstractDet : DETERMINANT =
  struct
  open C.Dom
  type tdet = v ref
  (* the first part of the state is an integer: which is +1, 0, -1:
     the sign of the determinant *)
  type 'a lstate = ('a,int ref) abstract * ('a,tdet) abstract
  type 'pc pc_constraint = unit
    constraint 'pc = <state : [> `TDet of 'a lstate ]; classif : 'a;..>
  type ('pc,'v) lm = ('pc,'v) cmonad
    constraint _  = 'pc pc_constraint
  type ('pc,'v) om = ('pc,'v) omonad
    constraint _  = 'pc pc_constraint
  type 'pc nm = ('p,unit) monad
    constraint _ = ('pc,'p,_) cmonad_constraint
    constraint _ = 'pc pc_constraint
  let ip = (fun x -> `TDet x), (function `TDet x -> Some x | _ -> None), "Det"
(* check later: XXX
  include Foo(struct type 'a tags = private [> `TDet of 'a lstate ] end)
*)
  let decl () =
      let* magn = retN (liftRef oneL) in    (* track magnitude *)
      let* sign = retN (liftRef Idx.one) in (* track the sign: +1, 0, -1 *)
      mo_extend ip (sign,magn)
  let upd_sign () =
      let* (sign,_) = mo_lookup ip in
      ret (Some (assign sign (Idx.uminus (liftGet sign))))
  let zero_sign () =
      let* (sign,_) = mo_lookup ip in
      assignM sign Idx.zero
  let get_magn () =
      let* (_,magn) = mo_lookup ip in
      ret magn
  let set_magn v =
      let* magn = get_magn () in
      assignM magn v
  let acc_magn v =
      let* magn = get_magn () in
      let* r = ret ((liftGet magn) *^ v) in
      assignM magn r
  let fin = fun () ->
      let* (sign,magn) = mo_lookup ip in
      ifM (Logic.equalL (liftGet sign) Idx.zero) (ret zeroL)
      (ifM (Logic.equalL (liftGet sign) Idx.one) (ret (liftGet magn))
          (ret (uminusL (liftGet magn))))
end

end

end
