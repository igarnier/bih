open Types

(* the camera is defined by its position, its rotation matrix and its "eye-to-screen" distance,
 * which specifies indirectly the angle (a small eye-to-screen entails a wide camera angle). *)
type t = 
  { pos     : Float3.t;
    rot     : Mat3.t;
    eyedist : float }

let init ~position ~eyedist ~angle ~axis =
  { pos     = position;
    rot     = Mat3.rot angle axis;
    eyedist = eyedist }



let precompute_rays_graphics xres yres ({ eyedist; pos; rot } as camera) =
  (* the normals are built in camera space, from the eye to each pixel and then normalized,
     and finally warped back to the global frame *)
  let rays = Array.make_matrix xres yres { origin  = Float3.zero; 
                                           normal  = Float3.i; 
                                           inormal = Float3.zero }
  in
  for x = 0 to xres - 1 do
    for y = 0 to yres - 1 do
      let ray_vec = [| (float x) -. (float (xres/2));
                       (float y) -. (float (yres/2));
                       camera.eyedist |] in
      let ray_normal = Mat3.apply_to_vec camera.rot (Float3.unit ray_vec) in
      let reciprocal =
        [| 1.0 /. ray_normal.(0);
           1.0 /. ray_normal.(1);
           1.0 /. ray_normal.(2) |] in
      rays.(x).(yres - 1 - y) <- { origin  = camera.pos;
                                   normal  = ray_normal;
                                   inormal = reciprocal }
    done
  done;
  rays
