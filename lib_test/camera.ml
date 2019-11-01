open Gg
open Types

(* the camera is defined by its position, its rotation matrix and its "eye-to-screen" distance,
 * which specifies indirectly the angle (a small eye-to-screen entails a wide camera angle). *)
type t = {pos : V3.t; rot : M3.t; eyedist : float}

let init ~position ~eyedist ~angle ~axis =
  {pos = position; rot = M3.rot3_axis axis angle; eyedist}

let precompute_rays_graphics xres yres camera =
  (* the normals are built in camera space, from the eye to each pixel and then normalized,
     and finally warped back to the global frame *)
  let rays =
    Array.make_matrix
      xres
      yres
      {origin = V3.zero; normal = V3.ox; inormal = V3.zero}
  in
  for x = 0 to xres - 1 do
    for y = 0 to yres - 1 do
      let ray_vec =
        V3.v
          (float x -. float (xres / 2))
          (float y -. float (yres / 2))
          camera.eyedist
      in
      let ray_normal = V3.ltr camera.rot (V3.unit ray_vec) in
      let reciprocal =
        V3.v
          (1.0 /. V3.x ray_normal)
          (1.0 /. V3.y ray_normal)
          (1.0 /. V3.z ray_normal)
      in
      rays.(x).(yres - 1 - y) <-
        {origin = camera.pos; normal = ray_normal; inormal = reciprocal}
    done
  done ;
  rays
