open Types

let create_shadow_ray normal hit light =
  let ray_target = Float3.add hit (Float3.scale normal 0.1) in
  let vec        = Float3.sub ray_target light.position in
  let length     = Float3.length vec in
  let ilength    = 1.0 /. length in
  let dir        = Float3.scale vec ilength in
  let reciprocal =
    [| 1.0 /. dir.(0);
       1.0 /. dir.(1);
       1.0 /. dir.(2) |] in  
  ({ origin = light.position;
     normal = dir;
     inormal = reciprocal }, ilength)

(* Recursive ray-tracing, whitted-style. Note that the shading is not a exactly standard
   Phong model (speculars not implemented). *)
let rec raytrace maxdepth scene tree tri_index ray =
  if maxdepth <= 0 then
    (* background_shader ray *)
    [| 0.0; 0.0; 0.0 |]
  else
    match Traverse.traverse scene tri_index ray 1.0 max_float tree with
    | Traverse.NoHit ->
      (* background_shader ray *)
      [| 0.0; 0.0; 0.0 |]
    | Traverse.Hit(triangle_id, { t; u; v }) ->
      let tri_norm = scene.nbuffer.(triangle_id) in
      let material = scene.materials.(scene.tbuffer.(triangle_id).mat) in
      (* compute reflection and shadow rays *)
      let hitpoint = Float3.add ray.origin (Float3.scale ray.normal t) in
      let dotprod  = ~-. 2.0 *. (Float3.dot tri_norm ray.normal) in
      let refl_dir = Float3.add ray.normal (Float3.scale tri_norm dotprod) in
      let reciprocal =
        [| 1.0 /. refl_dir.(0);
           1.0 /. refl_dir.(1);
           1.0 /. refl_dir.(2) |] in
      let refl_ray = { origin  = (Float3.add hitpoint (Float3.scale tri_norm 1.0));
                       normal  = refl_dir;
                       inormal = reciprocal } in
      let illumination = ref Float3.zero in
      for i = 0 to Array.length scene.lights - 1 do
        let light = scene.lights.(i) in
        let (shadow_ray, ilength) = create_shadow_ray tri_norm hitpoint light in
        if Traverse.traverse_shadow scene tri_index shadow_ray 0.0 max_float tree then
          let light_color   = Float3.scale light.color (abs_float (Float3.dot tri_norm shadow_ray.normal) *. light.intensity *. ilength (* *. ilength *)) in
          let result_color  = Float3.mult material.m_color (Float3.scale light_color material.m_diffuse) in
          illumination := (Float3.add !illumination result_color)
        else ()
      done;
      (* let _ = if !illumination.(0) <> 0.0 then Printf.printf "%f\n" !illumination.(0) else () in *)
      let reflect_color = raytrace (maxdepth - 1) scene tree tri_index refl_ray in
      let reflect_color = Float3.mult material.m_color (Float3.scale reflect_color material.m_diffuse) in
      Float3.add reflect_color !illumination

let renderfunc_bih xres yres scene tree tri_index =
  let camera =
    let position = [| 0.0; 0.0; -. 20.0 |] in
    Camera.init ~position ~eyedist:(float xres *. 0.5) ~angle:0.0 ~axis:Float3.i
  in
  let rays = Camera.precompute_rays_graphics xres yres camera in
  for x = 0 to xres - 1 do
    for y = 0 to yres - 1 do
      let ray   = rays.(x).(y) in
      let color = raytrace 1 scene tree tri_index ray in
      let r = if color.(0) > 1.0 then 255 else (int_of_float (color.(0) *. 255.0)) in
      let g = if color.(1) > 1.0 then 255 else (int_of_float (color.(1) *. 255.0)) in
      let b = if color.(2) > 1.0 then 255 else (int_of_float (color.(2) *. 255.0)) in
      let _ = assert (x >= 0 && x < xres) in
      let _ = assert (y >= 0 && y < yres) in
      (Graphics.set_color (Graphics.rgb r g b);
       Graphics.plot x (yres - y))
    done
  done

let main xres yres =
  let mat0 = {
    m_color    = [| 0.2; 0.15; 0.2 |];
    m_diffuse  = 1.0;
    m_specular = 1.0;
    m_shininess = 1.0
  } in
  let mat1 = {
    m_color    = [| 0.5; 0.15; 0.2 |];
    m_diffuse  = 1.0;
    m_specular = 1.0;
    m_shininess = 1.0
  } in
  let mat2 = {
    m_color    = [| 0.1; 0.5; 0.15 |];
    m_diffuse  = 1.0;
    m_specular = 1.0;
    m_shininess = 1.0
  } in
  let mirror = {
    m_color    = [| 0.9; 0.9; 0.9 |];
    m_diffuse  = 1.0;
    m_specular = 1.0;
    m_shininess = 1.0
  } in
  let scene  = {
    Scene.empty with
    ambient = [| 0.0; 0.0; 0.0 |];
    lights  = [| { position  = [| -. 10.0; 45.0; 30.0 |]; (*[| 0.0; 10.0; 30.0 |]; *)
                   intensity = 30.0;
                   color     = [| 1.0; 1.0; 1.0 |]
                 } |];
    materials = [| mat0; mat1; mat2; mirror |]
  } in
  let scene  =
    Wavefront.load
      ~filename:Sys.argv.(1)
      ~scale:3.0
      ~material:3
      ~position:[| 0.0; 0.0; 0.0 |]
      ~rotation:(Mat3.rot (acos ~-. 1.0) Float3.j)
      ~scene
  in
  let scene = Scene.recenter scene in
  let _      = Printf.printf "effective scene bbox %s\n%!" (Bih.Aabb.print scene.bbox) in
  let state, tree = Traverse.Tree.build scene 5 scene.tbuffer in
  let open Graphics in
  open_graph (Printf.sprintf " %dx%d" xres yres);
  auto_synchronize false;
  let frames = 5 in
  for i = 0 to frames - 1 do
    renderfunc_bih xres yres scene tree state.index;
    synchronize ()
  done;
  close_graph ()

let _ =
  Printf.printf "%s\n" (Unix.getcwd ())

let _ = main 800 600
