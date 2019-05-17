open Math

type sphere = {
  center : vec3;
  radius : float;
}

type plane = {
  p : vec3;
  normal : vec3;
}

type geometry =
  | Sphere of sphere
  | Plane of plane

type ray = {
  p0 : vec3;
  dir : vec3;
}

type hit_info = {
  point : vec3;
  normal : vec3;
  distance : float;
}

let raycast_sphere (r : ray) (s : sphere) : hit_info option =
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

let raycast_plane (r : ray) (p : plane) : hit_info option =
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

let raycast_geometry (r : ray) (g : geometry) : hit_info option =
  match g with
  | Sphere s -> raycast_sphere r s
  | Plane p -> raycast_plane r p
