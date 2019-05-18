module State = struct
  type t = {
    mutable state: int64;
    mutable inc: int64;
  }

  let create () = {state = 0L; inc = 0L}
end

type t = State.t

let update_state (rng:State.t) =
  rng.state <- Int64.(add (mul rng.state 0x5851f42d4c957f2dL) rng.inc)

let uniform_int32 rng =
  let old_s = rng.State.state in
  update_state rng;
  let (lsr) = Int64.shift_right_logical
  and (lxor) = Int64.logxor in
  let xor_shifted =
    ((old_s lsr 18) lxor old_s) lsr 27 |>
    Int64.to_int32
  and rot = Int64.to_int (old_s lsr 59) in
  let (lsl) = Int32.shift_left
  and (lsr) = Int32.shift_right_logical
  and (lor) = Int32.logor in
  (xor_shifted lsr rot) lor (xor_shifted lsl ((-rot) land 31))

let create seq_index =
  let s = State.create () in
  s.inc <- Int64.(logor (shift_left seq_index 1) 1L);
  update_state s;
  s.state <- Int64.(add s.state 0x853c49e6748fea9bL);
  update_state s;
  s

let one_minus_eps = (1. -. epsilon_float)

let uniform_uint32 rng =
  let i = Int32.to_int (uniform_int32 rng) in
  if i > 0 then i
  else -i

let uniform_float rng a =
  let random_int = uniform_uint32 rng in
  let random_float = (float_of_int random_int /. float_of_int (Int32.to_int Int32.max_int)) in
  if one_minus_eps < random_float then one_minus_eps
  else random_float
