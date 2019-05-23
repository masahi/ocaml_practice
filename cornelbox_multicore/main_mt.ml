open Math
open Render

let main () =
  let width = 300 in
  let height = 300 in
  let bounces = 5 in
  let paths_per_pixel = 100 in
  let cam = {
    transform = {
      translation = { x = 0.0; y = -30.0; z = 10.0; };
      linear = {
        right   = { x = 1.0; y = 0.0; z = 0.0; };
        up      = { x = 0.0; y = 1.0; z = 0.0; };
        forward = { x = 0.0; y = 0.0; z = 1.0; };
      };
    };
    frame_width = 100.0;
    aspect_ratio = 1.0;
    distance_to_frame = 100.0;
  } in
  let objs = [
    {
      geometry = Geom.sphere ~center:{ x = -35.0; y = -60.0; z = 200.0; } ~radius:25.0;
      material = Lambertian {r = 1.0; g = 1.0; b = 1.0; };
    };
    {
      geometry = Geom.sphere ~center:{ x =  35.0; y = -60.0; z = 200.0;} ~radius:25.0;
      material = Lambertian {r = 1.0; g = 1.0; b = 1.0; };
    };
    {
      geometry = Geom.plane ~pos:{ x = 0.0; y = -85.; z = 0.0; } ~normal:{ x = 0.0; y = 1.0; z = 0.0; };
      material = Lambertian { r = 1.0; g = 1.0; b = 1.0; };
    };
    {
      geometry = Geom.plane ~pos:{ x = 80.0; y = 0.0; z = 0.0; } ~normal:{ x = -1.0; y = 0.0; z = 0.0; };
      material = Lambertian { r = 0.0; g = 1.0; b = 0.0; };
    };
    {
      geometry = Geom.plane ~pos:{ x = -80.0; y = 0.0; z = 0.0; } ~normal:{ x = 1.0; y = 0.0; z = 0.0; };
      material = Lambertian { r = 1.0; g = 0.0; b = 0.0; };
    };
    {
      geometry = Geom.plane ~pos:{ x = 0.0; y = 0.0; z = 300.0; } ~normal:{ x = 0.0; y = 0.0; z = -1.0; };
      material = Lambertian { r = 1.0; g = 1.0; b = 1.0; };
    };
    {
      geometry = Geom.plane ~pos:{ x = 0.0; y = 90.0; z = 0.0; } ~normal:{ x = 0.0; y = -1.0; z = 0.0; };
      material = Light { r = 1.0; g = 1.0; b = 1.0; };
    };
  ] in
  let settings = {
    pixel_size_x = cam.frame_width /. (float_of_int width);
    pixel_size_y = (ceil (cam.frame_width /. cam.aspect_ratio)) /. (float_of_int height);
    bounces = bounces;
    paths_per_pixel = paths_per_pixel;
  } in
  let oc = open_out "render.ppm" in begin
    Printf.fprintf oc "P6\n%d %d\n255\n" width height;
    let num_thread = 6 in
    let height_per_thread = height / num_thread in
    let buffer = Array.init (height * width) (fun _ -> { r = 0.0; g = 0.0; b = 0.0 }) in

    let thread_func thread_id =
      Domain.spawn (fun () ->
          let y_start = height_per_thread * thread_id in
          let y_end = height_per_thread * (thread_id + 1) in
          List_aux.iota y_start y_end |> List.iter (fun y ->
              List_aux.iota 0 width |> List.iter (fun x ->
                  let t1 = (float_of_int x) /. (float_of_int (width - 1)) in
                  let t2 = (float_of_int y) /. (float_of_int (height - 1)) in
                  let color = render_pixel cam t1 t2 settings objs (Pcg.create (Int64.of_int (x + y * width))) in
                  buffer.((height - 1 - y) * width + x) <- color;))) in

    List_aux.iota 0 num_thread |> List.map (fun i -> thread_func i) |>  List.iter (fun d -> Domain.join d);

    Array.iter (fun c -> output_color oc c) buffer;

    output_char oc '\n';
    flush oc;
    close_out oc;
  end

let () = main ()
