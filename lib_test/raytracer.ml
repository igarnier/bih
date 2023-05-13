open Types
open Gg

let create_shadow_ray normal hit light =
  let ray_target = V3.add hit (V3.smul 0.1 normal) in
  let vec = V3.sub ray_target light.position in
  let length = V3.norm vec in
  let ilength = 1.0 /. length in
  let dir = V3.smul ilength vec in
  let reciprocal = V3.v (1.0 /. V3.x dir) (1.0 /. V3.y dir) (1.0 /. V3.z dir) in
  ({ origin = light.position; normal = dir; inormal = reciprocal }, ilength)

(* Recursive ray-tracing, whitted-style. Note that the shading is not a exactly standard
   Phong model (speculars not implemented). *)
let rec raytrace maxdepth scene tree tri_index ray =
  if maxdepth <= 0 then (* background_shader ray *)
    V3.zero
  else
    match Traverse.traverse scene tri_index ray 1.0 max_float tree with
    | Traverse.NoHit ->
        (* background_shader ray *)
        V3.zero
    | Traverse.Hit (triangle_id, { t; _ }) ->
        let tri_norm = scene.nbuffer.(triangle_id) in
        let material = scene.materials.(scene.tbuffer.(triangle_id).mat) in
        (* compute reflection and shadow rays *)
        let hitpoint = V3.add ray.origin (V3.smul t ray.normal) in
        let dotprod = ~-.2.0 *. V3.dot tri_norm ray.normal in
        let refl_dir = V3.add ray.normal (V3.smul dotprod tri_norm) in
        let reciprocal =
          V3.v
            (1.0 /. V3.x refl_dir)
            (1.0 /. V3.y refl_dir)
            (1.0 /. V3.z refl_dir)
        in
        let refl_ray =
          { origin = V3.add hitpoint tri_norm;
            normal = refl_dir;
            inormal = reciprocal
          }
        in
        let illumination = ref V3.zero in
        for i = 0 to Array.length scene.lights - 1 do
          let light = scene.lights.(i) in
          let (shadow_ray, ilength) =
            create_shadow_ray tri_norm hitpoint light
          in
          if
            Traverse.traverse_shadow
              scene
              tri_index
              shadow_ray
              0.0
              max_float
              tree
          then
            let light_color =
              V3.smul
                (abs_float (V3.dot tri_norm shadow_ray.normal)
                *. light.intensity *. ilength (* *. ilength *))
                light.color
            in
            let result_color =
              V3.mul material.m_color (V3.smul material.m_diffuse light_color)
            in
            illumination := V3.add !illumination result_color
          else ()
        done ;
        (* let _ = if !illumination.(0) <> 0.0 then Printf.printf "%f\n" !illumination.(0) else () in *)
        let reflect_color =
          raytrace (maxdepth - 1) scene tree tri_index refl_ray
        in
        let reflect_color =
          V3.mul material.m_color (V3.smul material.m_diffuse reflect_color)
        in
        V3.add reflect_color !illumination

let renderfunc_bih xres yres scene tree tri_index =
  let camera =
    let position = V3.v 0.0 0.0 ~-.20.0 in
    Camera.init ~position ~eyedist:(float xres *. 0.5) ~angle:0.0 ~axis:V3.ox
  in
  let rays = Camera.precompute_rays_graphics xres yres camera in
  for x = 0 to xres - 1 do
    for y = 0 to yres - 1 do
      let ray = rays.(x).(y) in
      let color = raytrace 1 scene tree tri_index ray in
      let r =
        if V3.x color > 1.0 then 255 else int_of_float (V3.x color *. 255.0)
      in
      let g =
        if V3.y color > 1.0 then 255 else int_of_float (V3.y color *. 255.0)
      in
      let b =
        if V3.z color > 1.0 then 255 else int_of_float (V3.z color *. 255.0)
      in
      let _ = assert (x >= 0 && x < xres) in
      let _ = assert (y >= 0 && y < yres) in
      Graphics.set_color (Graphics.rgb r g b) ;
      Graphics.plot x (yres - y)
    done
  done

let main xres yres =
  let mat0 =
    { m_color = V3.v 0.2 0.15 0.2;
      m_diffuse = 1.0;
      m_specular = 1.0;
      m_shininess = 1.0
    }
  in
  let mat1 =
    { m_color = V3.v 0.5 0.15 0.2;
      m_diffuse = 1.0;
      m_specular = 1.0;
      m_shininess = 1.0
    }
  in
  let mat2 =
    { m_color = V3.v 0.1 0.5 0.15;
      m_diffuse = 1.0;
      m_specular = 1.0;
      m_shininess = 1.0
    }
  in
  let mirror =
    { m_color = V3.v 0.9 0.9 0.9;
      m_diffuse = 1.0;
      m_specular = 1.0;
      m_shininess = 1.0
    }
  in
  let scene =
    { Scene.empty with
      ambient = V3.v 0.0 0.0 0.0;
      lights =
        [| { position = V3.v ~-.10.0 45.0 30.0;
             (*[| 0.0; 10.0; 30.0 |]; *)
             intensity = 30.0;
             color = V3.v 1.0 1.0 1.0
           }
        |];
      materials = [| mat0; mat1; mat2; mirror |]
    }
  in
  let scene =
    Wavefront.load
      ~filename:Sys.argv.(1)
      ~scale:3.0
      ~material:3
      ~position:V3.zero
      ~rotation:(M3.rot3_axis V3.oy (acos ~-.1.0))
      ~scene
  in
  let scene = Scene.recenter scene in
  let _ =
    Printf.printf "effective scene bbox %s\n%!" (Bih.Aabb.print scene.bbox)
  in
  let (state, tree) = Traverse.Tree.build scene 5 scene.tbuffer in
  let open Graphics in
  open_graph (Printf.sprintf " %dx%d" xres yres) ;
  auto_synchronize false ;
  let frames = 5 in
  for _i = 0 to frames - 1 do
    renderfunc_bih xres yres scene tree state.index ;
    synchronize ()
  done ;
  close_graph ()

let _ = Printf.printf "%s\n" (Unix.getcwd ())

let _ = main 800 600
