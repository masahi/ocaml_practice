type vec3 = {
  x : float;
  y : float;
  z : float;
}

type linear3 = {
  forward : vec3;
  right : vec3;
  up : vec3;
}

type transform3 = {
  translation : vec3;
  linear : linear3;
}

let ( @+ ) a b = { x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z }
let ( @- ) a b = { x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z }

let ( @* ) v s = { x = v.x *. s; y = v.y *. s; z = v.z *. s }
let ( @/ ) v s = { x = v.x /. s; y = v.y /. s; z = v.z /. s }

let dot a b = a.x *. b.x +. a.y *. b.y +. a.z *. b.z
let quadrance a = dot a a
let norm a = sqrt (quadrance a)
let normalize a = a @* (1.0 /. (norm a))

let cross a b = {
  x = -. a.z *. b.y +. a.y *. b.z;
  y =    a.z *. b.x -. a.x *. b.z;
  z = -. a.y *. b.x +. a.x *. b.y;
}

let apply_linear3 (l : linear3) (p : vec3) =
  let b1 = l.right in
  let b2 = l.up in
  let b3 = l.forward in
  {
    x = p.x *. b1.x +. p.y *. b2.x +. p.z *. b3.x;
    y = p.x *. b1.y +. p.y *. b2.y +. p.z *. b3.y;
    z = p.x *. b1.z +. p.y *. b2.z +. p.z *. b3.z;
  }

let apply_transform3 (t : transform3) (p : vec3) : vec3 =
  (apply_linear3 t.linear p) @+ t.translation

type quadratic_solution =
  | NoSolution
  | OneSolution of float
  | TwoSolutions of float * float

let solve_quadratic (a : float) (b : float) (c : float) : quadratic_solution =
  let d = b *. b -. 4.0 *. a *. c in
  if d >= 0.0 && a != 0.0
  then
    let sqrtd = sqrt d in
    if sqrtd = 0.0
    then
      OneSolution (-. b /. (2.0 *. a))
    else
      let t1 = (-. b +. sqrtd) /. (2.0 *. a) in
      let t2 = (-. b -. sqrtd) /. (2.0 *. a) in
      TwoSolutions (t1, t2)
  else NoSolution
