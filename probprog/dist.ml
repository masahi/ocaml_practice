type prob = float

type _ primitive =
  | Normal : float * float -> float primitive
  | Bernoulli : float -> bool primitive
  | UniformD : 'a list -> 'a primitive
  | Categorical: ('a * prob) list -> 'a primitive

type _ t =
  | Return : 'a -> 'a t
  | Bind: 'b t * ('b -> 'a t) -> 'a t
  | Primitive : 'a primitive -> 'a t
  | Conditional : ('a -> prob) * 'a t -> 'a t

let bind d f = Bind(d, f)
let return x = Return(x)
let map d f = Bind(d, (fun x -> return (f x)))
let normal mu sigma = Primitive(Normal(mu, sigma))
let std_normal () = normal 0.0 1.0
let bernoulli p = Primitive(Bernoulli(p))
let categorical weighted_choices = Primitive(Categorical(weighted_choices))

module Let_Syntax = struct
  let (let*) d f = bind d f
  let (let+) d f = map d f
end
