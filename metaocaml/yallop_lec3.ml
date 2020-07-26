open Codelib
(** Generating mutual recursion

    Staging in MetaOCaml is centered around *expressions*.  Both
    quotations and antiquotations (splices) operate on expressions:

      .< expression >.
      .~expression

    and can be used anywhere an expression can occur:

       let x = expr in expr          (* let binding *)
       expr expr                     (* function application *)
       if expr then expr else expr   (* branching *)
       C expression                  (* constructor application *)
       (etc.)

    Generating programs with these constructs in a compositional way
    is straightforward: one part of the generating program builds
    an expression, then another part of the program inserts the
    generated expression into a template.

    For example, here is the staged pow function from lecture 14:
*)
let rec pow x n =
  if n = 0 then .< 1 >.
  else .< .~x * .~(pow x (n - 1)) >.
(**
   The pow function builds up what looks like an n-ary product:

      x * x * x * 1

   In fact, the product is constructed one step at a time from smaller
   products; each component is an expression:

      x * (x * (x * 1))
*)

(**
   Here is a similar function, x2n, that builds specialized functions
   that compute xÃ—2â¿:
*)
let rec x2n n acc =
  if n = 0 then acc
  else .< let x = .~acc * 2 in .~(x2n (pred n) .<x>.) >.
(**
   This time the generated code is a sequence of 'let' bindings:

    .< fun y -> .~(x2n 3 .<y>.) >.
   â†
    .< fun y  ->
        let xâ‚ = y * 2 in
        let xâ‚‚ = xâ‚ * 2 in
        let xâ‚ƒ = xâ‚‚ * 2 in
        xâ‚ƒ>.

   Again, generating this code is easy with MetaOCaml's expression-centred
   approach since these let bindings can be constructed one step at a time
   from  smaller expressions:

        let xâ‚ = y * 2 in
        (let xâ‚‚ = xâ‚ * 2 in
         (let xâ‚ƒ = xâ‚‚ * 2 in
           xâ‚ƒ))

   Other constructs, such as ladders of if/else tests, can be similarly
   built up one step at a time.
*)

(**
   Mutual recursion is a common feature of functional programs:

      let rec fâ‚ = eâ‚
          and fâ‚‚ = eâ‚‚
          and fâ‚ƒ = eâ‚ƒ

   However, unlike the constructs considered above, mutually-recursive
   bindings are not built up from smaller expressions, and cannot
   be constructed one step at a time.  There is no way to write a function
   that takes an integer n and constructs a set of n mutually-recursive
   bindings using MetaOCaml quotations and splices.

   These notes show the design and behaviour of an addition to MetaOCaml
   that supports generating mutually-recursive bindings.
*)

(** We'll start by looking at 'fixed point' operators, which are sometimes
    used in functional programs to define recursive functions.  Fixed
    point operators are an instance of a common pattern in this course:
    looking at how to implement built-in functionality in the language
    with user-defined variants.  Previous examples include

     * replacing '+' with a partially-static variation
       in order to generate code with better performance

     * using the monadic bind '>>=' in place of 'let'
       in order to use new types of effect

     * defining the built-in type equality 'â‰¡' as a library
       making it possible to use other relations such as subtyping
       in a similar way.

    This time we'll look at how user-defined fixed point operators allow us
    to extend the behaviour of 'let rec', ultimately allowing us to
    write staged programs that generate mutually-recursive bindings.
*)

(**

   In an earlier lecture (on semirings) we saw how a type-level
   fixed-point operator (written Î¼) can be used to construct
   recursively-defined types.  For example here is a definition of
   integer lists

      type intlist =
        Nil : intlist
      | Cons : int * intlist -> intlist

   and here is a corresponding type expression using Î¼:

       Î¼â„“.1 + (int Ã— â„“)

   The Î¼ operator binds a variable â„“ that can be used in the body to
   refer to the whole type expression.  In the example above, the
   parts of the type expression correspond to the definition of 'intlist'
   as follows:

      * 'Î¼â„“.' corresponds to 'type intlist':
        it introduces a name (â„“ or intlist) that can be used to refer to
        the whole definition

      * '+' corresponds to '|'
        it defines labeled sums with constructors inl/inr or Nil/Cons

      * '1' corresponds to the arguments of 'Nil':
        (there are no arguments; we represent this as unit)

      * (int Ã— â„“) corresponds to the argument pair of 'Cons':
        there is an integer argument for the head
        and an intlist argument for the tail

   The equation for Î¼ looks like this:

      Î¼â„“.A  = A[â„“:=Î¼â„“.A]

   It says that we can replace occurrences of â„“ with the whole
   definition Î¼â„“.A within the body, A â€”- i.e. we can unroll
   the definition one step at a time.
*)

(** The fixed point equation translates into a function definition
    in a functional language.  Here is a definition of a function
    'fix' in Haskell:

       fix :: (a -> a) -> a
       fix f = f (fix f)

    The function 'f' corresponds to the body A of 'Î¼â„“.A', and
    the right-hand side corresponds to the replacement of â„“
    (i.e. the argument of f) with the full definition (fix f).

    The 'fix' function is recursive â€“- in fact, it captures
    the essence of recursive behaviour.  Using 'fix' we
    can write recursive functions without using additional
    recursion.

    Here is an example: the 'len' function computes the length of a
    list recursively:

       len [] = 0
       len (_:t) = 1 + len t

    (This corresponds to the OCaml code
       let rec len = function [] -> 0 | _ :: t -> 1 + len t
    )

    Using 'fix' we can write 'len' without recursion.  There are two
    steps.  First, rewrite len to remove the recursion by adding a
    'self' argument, and replacing each recursive reference to 'len'
    with 'self':

       len' self [] = 0
       len' self (_:t) = 1 + self t

    Then, restore the recursive behaviour using 'fix':

       len = fix len'

    Here is the behaviour of this second definition on a list of length 2:

         len ('a' : 'b' : [])

      â†  (by the definition of len: len = fix len')
         fix len' ('a' : 'b' : [])}

      â†  (by the definition of fix: fix f = f (fix f))
         len' (fix len') ('a' : 'b' : [])

      â†  (by the definition of len': len' self (_:t) = 1 + self t)
         1 + len' (fix len') ('b' : [])

      â†  (by the definition of len': len' self (_:t) = 1 + self t)
         1 + (1 + (len' (fix len') []))

      â†  (by the definition of len': len' self [] = 0)
         1 + (1 + 0)

      â†  (by the definition of +)
         2

    Observe: this evaluation relies on the lazy (CBN) behaviour of
    Haskell, since the third step reduces

        len' (fix len') ('a' : 'b' : [])

    to

        1 + len' (fix len') ('b' : [])

    *without* first reducing the argument fix len'.

    The above definition of 'fix' cannot be translated directly in
    OCaml.  Here is an attempt:
*)
(* val fix : ('a -> 'a) -> 'a *)
let rec fix f = f (fix f)
(** And here is a definition of 'len' using 'fix': *)
let len' self = function
  | []   -> 0
  | _::t -> 1 + self t

(* let len = fix len' *)

(** Let's trace the evaluation to see what goes wrong.

    len ('a'::'b'::[])

  â† (by the definition of len: len = fix len')
    fix len' ('a'::'b'::[])

  â† (by the definition of fix: fix f = f (fix f))
    len' (fix len') ('a'::'b'::[])}}

  â† (by the definition of fix: fix f = f (fix f))
    len' (len' (fix len')) ('a'::'b'::[])}}

  â† (by the definition of fix: fix f = f (fix f))
    len' (len' (len' (fix len'))) ('a'::'b'::[])}}

  â† ... (infinite regress!)

  OCaml's eager (CBV) behaviour means that the argument fix len
  must be reduced before the call to len'; this results in a sequence
  of reductions which never comes to an end.
*)

(** The solution is to *eta-expand* the fixed-point operator,
    adding an additional argument as follows.
    (Observe the change in the type!)
 *)
(* val fixV : (('a -> 'b) -> ('a -> 'b)) -> ('a -> 'b) *)
let rec fixV f x = f (fixV f) x

(** Now len can be written using fixV: *)
let lenV () = fixV len'

(** Here is the evaluation behaviour of this second definition:

      lenV ('a'::'b'::[])

   â†  (by the definition of lenV: lenV = fixV len')
      fixV len' ('a'::'b'::[])

   â†  (by the definition of fixV: fixV f = fun x -> f (fixV f) x)
      len' (fun x -> len' (fixV len') x) ('a'::'b'::[])

   â†  (by the definition of len': len' self (_::t) = 1 + self t)
      1 + (fun x -> len' (fixV len') x) ('b'::[])

   â†  (reducing the application of the 'fun')
      1 + len' (fixV len') ('b'::[])

   â†  (by the definition of fixV: fixV f = fun x -> f (fixV f) x)
      1 + len' (fun x -> len' (fixV len') x) ('b'::[])

   â†  (by the definition of len': len' self (_::t) = 1 + self t)
      1 + (1 + (fun x -> len' (fixV len') x) [])

   â†  (reducing the application of the 'fun')
      1 + (1 + (len' (fixV len') []))

   â†  (by the definition of fixV: fixV f = fun x -> f (fixV f) x)
      1 + (1 + (len' (fun x -> len' (fixV len') x) []))

   â†  (by the definition of len': len' self [] = 0)
      1 + (1 + 0)

   â†  (by the definition of +)
      2

   Observe: now that fixV has an extra argument, the call

      fixV f

   reduces to a value (fun x -> f (fixV f) x), and so the infinite
   regress is avoided.

   However the type of 'fixV' is less general than the type of 'fix':
   it can only be used to create recursive *functions*, not other
   type of recursive value.
*)

(**
   We have seen how to use fix and fixV to define recursive
   functions.  Let's consider how to define *mutually* recursive
   functions.

   The standard example of mutual recursion is the following
   definition of two functions that determine whether a function
   is even or odd
*)
(* let rec even n = n =  0 || odd  (n - 1)
 *     and odd  n = n <> 0 && even (n - 1) *)
(**
   Here are the equivalent Haskell definitions:

    even n = n == 0 || odd (n - 1)
    odd  n = n /= 0 && even (n - 1)

   In Haskell 'even' and 'odd' can be defined by instantiating
   the variable 'a' in the type of 'fix' to a pair type.  Here
   is the type of fix:

      fix :: (a -> a) -> a

   Instantiating the variable 'a' to the type of the pair
   (even, odd) gives

      fix :: ((Int -> Bool, Int -> Bool) -> (Int -> Bool, Int -> Bool))
           -> (Int -> Bool, Int -> Bool)

   Then the fix function can be used to simultaneously define
   the 'even' and 'odd' functions as a pair:

    (even, odd) = fix (Î»~(even, odd) ->
                      ((Î»n -> n == 0 || odd (n - 1)),
                       (Î»n -> n /= 0 && even (n - 1))))
 *)

(**
    How might we define 'even' and 'odd' using  'fixV'?
    Since fixV is restricted to constructing values of
    function type, the instantiation used in Haskell will
    not work.  Here is the type of fixV:

      val fixV : (('a -> 'b) -> ('a -> 'b)) -> ('a -> 'b)

    And here is the type of the pair (even, odd):

      (int -> bool) * (int -> bool)

    This second type is not an instance of the type 'a -> 'b.

    One simple (if slightly inelegant) solution is to add a unit
    argument, instantiating 'a -> 'b to unit -> (int -> bool) * (int ->
    bool).  Then we can write even and odd using fixV as follows:
 *)
let (_, _) =
  fixV (fun eo () ->
      ((fun n -> n =  0 || snd (eo ()) (n - 1)),
       (fun n -> n <> 0 && fst (eo ()) (n - 1))))
    ()

(** We will focus on an alternative approach, based on
    the type isomorphisms we saw in the semirings lecture.
    Recall that, interpreting the function arrow as
    exponentiation.

       a â†’ b  corresponds to  báµƒ

    Then we have the following type isomorphism:

       a Ã— a   â‰¡   aÂ²   â‰¡   2 â†’ a   â‰¡   bool â†’ a

    i.e. we can freely convert between homogeneous pairs
    and functions with boolean arguments.  For clarity
    we'll use the following two-constructor type in place
    of bool:
 *)
type eo = Even | Odd  (* isomorphic to bool *)

(** Now we can define even and odd as follows using fixV: *)
let eo = fixV (fun eo -> function
   Even -> (fun n -> n =  0 || eo Odd (n - 1))
 | Odd  -> (fun n -> n <> 0 && eo Even (n - 1)))

let even, odd = eo Even, eo Odd

(** So far we have seen how to build pairs of mutually-recursive
    functions.  However, the difficulty with mutual recursion and
    staging does not arise with pairs, but with binding groups
    of size n, where n is unknown.  Fortunately, our approach to
    defining 'even' and 'odd' generalizes straightforwardly to
    this case.

    The 'eo' function above generates two functions because
    the type used as argument has two constructors, Even and
    Odd.  Using a different type (with more constructors) as
    argument, we can build mutually-recursive groups of
    arbitrary size.

    Here is an example: the 'even' and 'odd' functions
    can be generalized to compute residuals modulo n:

      let rec fâ‚€ x = x  = 0 || fâ‚™â‚‹â‚ (x-1)
          and fâ‚ x = x <> 0 && fâ‚€ (x-1)
          ...
          and fâ‚™â‚‹â‚ x = x <> 0 && fâ‚™â‚‹â‚‚ (x-1)

    Now fáµ¢ x is true if x = i mod n; for example when n = 4 then
    fâ‚‚ 6 is true because 6 mod 4 = 2, but fâ‚ 3 is false because
    3 mod 4 â‰  1.

    Here is a definition of the residuals modulo n using fixV.
    Note that the "index type" (the first argument of fs) is
    no longer eo, but int, making it possible to build mutually
    recursive groups of arbitrary size:
 *)
let fs n = fixV (fun fs i ->
   if i = 0 then fun x -> x  = 0 || fs (n - 1) (x - 1)
   else          fun x -> x <> 0 && fs (i - 1) (x - 1))

(** Example: zero_mod_4 x is true if x mod 4 = 0 *)
let zero_mod_4 = fs 4 0

(**
   Computing residuals modulo n using mutual recursion is
   instructive, but not especially useful.

   Here is a more realistic example: it is convenient to
   define state machines with mutual recursion.  For example,
   here is a state machine with three states s1, s2, s3,
   and three transitions (labeled a, b, and c):

                    â€”bâ€”
        _____      _â†“_|_      _____
       |     |    |     |    |     |
      â†’| s1  |â€”aâ€”â†’| s2  |â€”câ€”â†’| s3  |â†’
       |_____|    |_____|    |_____|

   A natural way to define a state machine in OCaml is
   as a group of mutually-recursive functions, with
   one constructor for each state:
 *)
let rec s1 = function
  | 'a' :: k -> s2 k
  | _ -> failwith "no transition"
and s2 = function
  | 'b' :: k -> s2 k
  | 'c' :: k -> s3 k
  | _ -> failwith "no transition"
and s3 = function
  | [] -> true
  | _ -> false

(** However, it is sometimes inconvenient to write out the functions
    by hand; instead, we might like to build them from other data.
    Here the approach developed above comes in useful: we can construct
    the group without knowing beforehand how many members it contains.

    As with the even/odd and residual examples, we encode mutual recursion
    using indexed recursion.  The first step is to define an index type,
    with one constructor for each state:
 *)
type state = S1 | S2 | S3

(** Then we can construct the mutual recursion using fixV: *)
let s = fixV (fun s -> function
 | S1 -> (function 'a' :: k -> s S2 k
                 | _ -> failwith "no transition")
 | S2 -> (function 'b' :: k -> s S2 k
                 | 'c' :: k -> s S3 k
                 | _ -> failwith "no transition")
 | S3 -> (function [] -> true
                 | _ -> false))

(** (Exercise: define a type to represent state machines and implement the
    corresponding mutually-recursive functions using fixV) *)

(** We now consider how to stage 'fixV' in order to generate mutually-recursive
    bindings, addressing the difficulty discussed at the beginning of this
    file.

    As usual, we start with a simple binding-time analysis: which aspects of
    fixV should be considered dynamic, and which parts should be considered
    dynamic?  In order to generate a mutually-recursive group, the index
    values (e.g. Even and Odd, or S1, S2, S3) must be known during code
    generation, so we label these static; everything else may be dynamic.
    As a consequence, index values will not appear in generated code.

    Here is the type of the staged fixV under this analysis:

      val fixVS : (('a -> 'b code) -> ('a -> 'b code)) ->
                  ('a -> 'b code)

    And here is a simple implementation of fixVS:
    (The behaviour of the 'letrec' function is described further down.)
 *)

let fixVS f x = Letrec.letrec f (fun r -> r x)
(**
    And here is the type of fixVS specialized to the indexed type 'eo':

      fixVS : ((eo -> (int -> bool) code) -> (eo -> (int -> bool))) ->
              (eo -> (int -> bool) code)

    Here is a call to fixVS with 'eo' as the indexed type:
*)
let _ = fixVS (fun eo -> function
            | Even -> .< fun x -> x  = 0 || .~(eo Odd)  (x-1) >.
            | Odd  -> .< fun x -> x <> 0 && .~(eo Even) (x-1) >.)
(** Code is generated when the final argument, an index value, is supplied: *)
let evens = fixVS (fun eo -> function
                | Even -> .< fun x -> x  = 0 || .~(eo Odd)  (x-1) >.
                | Odd  -> .< fun x -> x <> 0 && .~(eo Even) (x-1) >.)
              Even
(** Here is the generated code for the above call:

  let rec x1 x = (x = 0) || (x2 (x - 1))
      and x2 x = (x <> 0) && (x1 (x - 1))
       in x1
 *)

(** (Exercise: can you generate mutually-recursive bindings for the
   state machine in a similar way?)  *)

(**
    The remaining steps in transforming fixV so that it generates
     mutually-recursive bindings are as follows:

     1. following the binding-time analysis, change the definition of 'fixV'
        so that it generates 'let rec' bindings rather than performing
        recursive calls directly.

     2. add memoization for indexes, so that multiple recursive calls using
        the same index generate a single binding.  For example, even if there
        are multiple calls to 'eo Even', only a single binding should be
        generated.

     3. add support for 'let rec' bodies.  A 'let rec' expression in OCaml
        consists of a set of bindings and a body.  For example, in the
        following expressions the bindings are 'xi = ei' and the body
        is the closing 'e':

            let rec x1 = e1
                and x2 = e2
                ...
                and xn = en
             in e

         A function that generates code for 'let rec' expressions must
         generate both bindings and arbitrary bodies.
 *)

(** Generating 'let rec' bodies.

    Let's look again at the type of the fixVS function:

      val fixVS : (('a -> 'b code) -> ('a -> 'b code)) ->
                  ('a -> 'b code)

    fixVS takes two arguments:

     * the first argument has the following type:

         ('a -> 'b code) -> ('a -> 'b code)

       i.e. the argument is a function that itself accepts two arguments:

           - an "resolver" function of type 'a -> 'b code
             that maps indexes to members of the recursive group

           - an index of type 'a

     * the second argument has type 'a
       i.e. it is an index that can be used to select a particular function
       from the recursive group.

    This is sufficient for generating functions such as 'evens' above
    where the body consists of a single variable from the recursive
    group.  However, to generate arbitrary bodies we need an function with
    a more general type.

    The 'letrec' function provides the extra generality we need.
    Here is its type:

      val letrec : (('a -> 'b code) -> ('a -> 'b code)) ->
                   (('a -> 'b code) -> 'c code) ->
                   'c code

    The last argument to 'fixVS' is an index value; the last argument to
    'letrec' is a function that builds code using a resolver function.
    This more general type allows 'letrec' to build 'let rec' binding
    groups with arbitrary bodies.

    Here is a definition of 'even' using 'letrec':
 *)
let evenlr = Letrec.letrec (fun eo -> function
  | Even -> .< fun x -> x  = 0 || .~(eo Odd)  (x-1) >.
  | Odd  -> .< fun x -> x <> 0 && .~(eo Even) (x-1) >.)
 (fun eo -> eo Even)

(** We can also generate binding groups whose bodies
    are more complex.  In the following example the body
    is a pair (even, odd) *)
let even_odd_lr = Letrec.letrec (fun eo -> function
  | Even -> .< fun x -> x  = 0 || .~(eo Odd)  (x-1) >.
  | Odd  -> .< fun x -> x <> 0 && .~(eo Even) (x-1) >.)
 (fun eo -> .< .~(eo Even), .~(eo Odd) >.)

let _ =
  print_code Format.std_formatter even_odd_lr; print_newline ()
(** (Exercise: can you generate mutually-recursive bindings for the
    state machine using Letrec.letrec?)  *)

(** Let's trace the behaviour of 'let rec' in the generation of evenlr.

    At a high level the behaviour is as follows:

    * The call to 'letrec' inserts a 'let rec' binding group

    * Each call to 'eo' adds a binding to the group

    * At most one binding is inserted for each index ('Even', 'Odd')

    That is, 'letrec' and 'eo' behave similarly to the let-insertion
    operations 'let_locus' and 'genlet' from lecture 15.

    Here is the behaviour of the following call in more detail:

      letrec (fun eo -> function
        | Even -> .< fun x -> x  = 0 || .~(eo Odd)  (x-1) >.
        | Odd  -> .< fun x -> x <> 0 && .~(eo Even) (x-1) >.)
       (fun eo -> eo Even)

    Step 1: letrec starts a binding group and invokes the body:

      .< let rec (* nothing *)
         .in ~(eo Even) >.

    Step 2: The call to 'eo Even' in the body inserts a binding 'xe';
        the right-hand side of the binding is the code from the case
        for Even in the function passed as argument to 'letrec':

      .< let rec xe = fun x -> x  = 0 || .~(eo Odd)  (x-1)
         .in ~((*eo Even*)) >.

        Meanwhile, the generation of the body is suspended, awaiting the
        return value of 'eo Even'.

    Step 3: eo Odd inserts a second binding 'xo'; the right-hand
       side is the code from the case for Odd; meanwhile the
       generation of the binding 'xe' is suspended, awaiting
       the return value of 'eo Odd':

      .< let rec xe = fun x -> x  = 0 || .~((*eo Odd*))  (x-1)
             and xo = fun x -> x <> 0 && .~(eo Even) (x-1)
         .in ~((*eo Even*)) >.

    Step 4: 'eo Even' resolves to the existing binding 'xe';
       The binding for 'xo' is now complete:

      .< let rec xe = fun x -> x  = 0 || .~((*eo Odd*))  (x-1)
             and xo = fun x -> x <> 0 && xe (x-1)
         .in ~((*eo Even*)) >.

    Step 5: Since the case for 'xo' is complete, the call 'eo Odd'
       returns the variable 'xo', which is inserted into the binding
       for 'xe':

      .< let rec xe = fun x -> x  = 0 || xo (x-1)
             and xo = fun x -> x <> 0 && xe (x-1)
         .in ~((*eo Even*)) >.

    Step 6: Finally, since the case for 'xe' is complete, the call
       'eo Even' in the body completes, resolving to the variable 'xe':

      .< let rec xe = fun x -> x  = 0 || xo (x-1)
             and xo = fun x -> x <> 0 && xe (x-1)
         .in xe >.
 *)

(** As shown in the slides, the letrec function can be generalized to
    support heterogeneous and polymorphic recursion.

    The 'Make' function in the Letrec module takes a parameterized
    index type together with a custom equality function, and builds a
    'letrec' function specialized to that index, where the type of
    generated code for each binding can vary with the type of the
    index:

       https://github.com/yallop/metaocaml-letrec/blob/f4672efb/lib/letrec.mli#L42

   (Exercise: define an implementation of Letrec.SYMBOL and use it together
    with Letrec.Make to build a staged polymorphic-recursive function)
*)
