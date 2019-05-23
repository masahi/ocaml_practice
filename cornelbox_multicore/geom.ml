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

module type Intf = sig
  type t
  val intersect_ray : t -> ray -> hit_info option
end

module type Object = sig
  module M: Intf
  val this: M.t
end

module Shapes = struct
  module Sphere = struct
    type t = {
      center : vec3;
      radius : float;
    }

    let create center radius = {center; radius}

    let intersect_ray s r =
      let a = (quadrance r.dir) in
      let b = 2.0 *. (dot r.dir (r.p0 @- s.center)) in
      let c = (quadrance r.p0) +. (quadrance s.center) -. 2.0 *. (dot s.center r.p0) -. (s.radius *. s.radius) in
      let ts =
        match (solve_quadratic a b c) with
        | NoSolution -> []
        | OneSolution t -> [t]
        | TwoSolutions (t1, t2) -> [t1; t2] in
      let filtered_ts = List.filter (fun t -> t > 0.0) ts in
      let sorted_ts = List.sort (fun a b -> (int_of_float (a -. b))) filtered_ts in
      let ot =
        match sorted_ts with
        | [] -> None
        | hd :: _ -> Some hd in
      match ot with
      | None -> None
      | Some t ->
        let hit_point = r.p0 @+ r.dir @* t in
        Some {
          point = hit_point;
          normal = (normalize (hit_point @- s.center));
          distance = t;
        }
  end

  module Plane = struct
    type t = {
      p : vec3;
      normal : vec3;
    }

    let create p normal = {p; normal}

    let intersect_ray p r =
      let d = (dot r.dir p.normal) in
      if d = 0.0 then
        None
      else
        let t = (dot p.normal (p.p @- r.p0)) /. d in
        if t > 0.0 then
          Some {
            point = r.p0 @+ (r.dir @* t);
            normal = p.normal;
            distance = t;
          }
        else
          None
  end
end

let sphere ~center ~radius=
  (module struct
    module M = Shapes.Sphere
    let this = M.create center radius
  end : Object)

let plane ~pos ~normal =
  (module struct
    module M = Shapes.Plane
    let this = M.create pos normal
  end : Object)
