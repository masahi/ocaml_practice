open Math
open Geom

type color = {
  r : float;
  g : float;
  b : float;
}

let ( %+ ) a b = { r = a.r +. b.r; g = a.g +. b.g; b = a.b +. b.b }
let ( %- ) a b = { r = a.r -. b.r; g = a.g -. b.g; b = a.b -. b.b }
let ( %. ) a b = { r = a.r *. b.r; g = a.g *. b.g; b = a.b *. b.b }

let ( %* ) a s = { r = a.r *. s; g = a.g *. s; b = a.b *. s }
let ( %/ ) a s = { r = a.r /. s; g = a.g /. s; b = a.b /. s }

type material =
  | Lambertian of color
  | Light of color

type scene_object = {
  material : material;
  geometry : geometry;
}

type camera = {
  transform : transform3;
  frame_width : float;
  aspect_ratio : float;
  distance_to_frame : float;
}

type render_settings = {
  pixel_size_x : float;
  pixel_size_y : float;
  paths_per_pixel : int;
  bounces : int;
}

let output_color oc color =
  let r = (char_of_int (int_of_float (color.r *. 255.0))) in
  let g = (char_of_int (int_of_float (color.g *. 255.0))) in
  let b = (char_of_int (int_of_float (color.b *. 255.0))) in begin
    output_char oc r;
    output_char oc g;
    output_char oc b;
  end

let cam_frame_local_to_world (cam : camera) (t1 : float) (t2 : float) =
  let w = cam.frame_width in
  let h = (ceil (w /. cam.aspect_ratio)) in
  let local_point = {
    x = -0.5 *. w *. (1.0 -. t1) +. 0.5 *. w *. t1;
    y = -0.5 *. h *. (1.0 -. t2) +. 0.5 *. h *. t2;
    z = cam.distance_to_frame
  } in
  (apply_transform3 cam.transform local_point)

let make_cam_ray (cam : camera) (t1 : float) (t2 : float) (pixel_size_x : float) (pixel_size_y : float) =
  let p = (cam_frame_local_to_world cam t1 t2) @+ {
    x = ((Random.float 1.0) -. 0.5) *. pixel_size_x;
    y = ((Random.float 1.0) -. 0.5) *. pixel_size_y;
    z = 0.0;
  } in
  {
    p0 = p;
    dir = normalize (p @- cam.transform.translation)
  }

let tbn_of_normal normal =
  let random_vec = (normalize {
    x = ((Random.float 2.0) -. 1.0);
    y = ((Random.float 2.0) -. 1.0);
    z = ((Random.float 2.0) -. 1.0)
  }) in
  let tangent = cross normal random_vec in
  let bitangent = cross tangent normal in
  (tangent, bitangent, normal)

let hemisphere_sample normal =
  match tbn_of_normal normal with
  | (t, b, n) ->
    (t @* ((Random.float 2.0) -. 1.0)) @+
    (b @* ((Random.float 2.0) -. 1.0)) @+
    (n @* (Random.float 1.0))

let rec raycast (r : ray) (objs : scene_object list) : (hit_info * scene_object) option =
  match objs with
  | [] -> None
  | obj :: rest ->
    match raycast_geometry r obj.geometry with
    | None -> raycast r rest
    | Some hit1 ->
      match raycast r rest with
      | None -> Some (hit1, obj)
      | Some (hit2, obj2) ->
        if hit1.distance < hit2.distance
        then Some (hit1, obj)
        else Some (hit2, obj2)

let rec path_trace_bounce (r : ray) objs bounces =
  if bounces = 0 then
    { r = 0.0; g = 0.0; b = 0.0 }
  else
    match raycast r objs with
    | None -> { r = 0.0; g = 0.0; b = 0.0 }
    | Some (hit, obj) ->
      match obj.material with
      | Lambertian c ->
        let new_ray = {
          p0  = hit.point @+ (hit.normal @* 10e-8);
          dir = hemisphere_sample hit.normal
        } in
        ((path_trace_bounce new_ray objs (bounces - 1)) %. c) %* 0.99
      | Light _ -> { r = 1.0; g = 1.0; b = 1.0; }

let rec path_trace_average
  cam t1 t2 settings objs
  paths_left partial_color =
  if paths_left = 0 then
    partial_color %/ (float_of_int settings.paths_per_pixel)
  else
    let r = make_cam_ray cam t1 t2 settings.pixel_size_x settings.pixel_size_y in
    let color = path_trace_bounce r objs settings.bounces in
    path_trace_average
      cam t1 t2 settings objs
      (paths_left - 1) (partial_color %+ color)

let render_pixel cam t1 t2 settings objs =
  path_trace_average
    cam t1 t2 settings objs
    settings.paths_per_pixel { r = 0.0; g = 0.0; b = 0.0 }
