open Math

type ray = {
  p0 : vec3;
  dir : vec3;
}

type hit_info = {
  point : vec3;
  normal : vec3;
  distance : float;
}

module type Object = sig
  module M: sig
    type t
    val intersect_ray: t -> ray -> hit_info option
  end
  val this: M.t
end

val sphere: center:vec3 -> radius:float -> (module Object)
val plane: pos:vec3 -> normal:vec3 -> (module Object)
