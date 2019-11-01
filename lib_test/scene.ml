open Gg
open Types

exception LoadError

(* The [scene] record contains the geometry, the light sources as well as some global lighting parameters *)
type t = {
  ambient : V3.t;
  (* uniform lighting color as (r,g,b) \in [0;1]^3 *)
  lights : light array;
  (* light sources *)
  materials : material array;
  (* surface properties *)
  vbuffer : V3.t array;
  (* vertex buffer *)
  (* - might be partially empty *)
  tbuffer : triangle array;
  (* triangle buffer *)
  (* - might be partially empty *)
  bbuffer : Bih.Aabb.t array;
  (* bounding boxes for triangles, precomputed at load-time, same length as [tbuffer] *)
  nbuffer : V3.t array;
  (* normals for triangles, precomputed at load-time, same length as [tbuffer] *)
  bbox : Bih.Aabb.t; (* scene bounding box *)
}

(* Empty scene *)
let empty =
  {
    ambient = V3.zero;
    materials = [||];
    lights = [||];
    vbuffer = [||];
    tbuffer = [||];
    bbuffer = [||];
    nbuffer = [||];
    bbox = Bih.Aabb.empty 3;
  }

let add_object_to_scene scene vbuffer (tbuffer : Types.triangle array)
    materials =
  let vbuffer' = Array.append scene.vbuffer vbuffer in
  let bbuffer =
    Array.map
      (fun {t1; t2; t3; _} ->
        Misc.aabb_of_triangle vbuffer'.(t1) vbuffer'.(t2) vbuffer'.(t3))
      tbuffer
  in
  let nbuffer =
    Array.map
      (fun {t1; t2; t3; _} ->
        V3.unit
          (V3.cross
             (V3.sub vbuffer'.(t2) vbuffer'.(t1))
             (V3.sub vbuffer'.(t3) vbuffer'.(t1))))
      tbuffer
  in
  let object_bbox = Array.fold_left Bih.Aabb.join (Bih.Aabb.empty 3) bbuffer in
  let new_scene =
    {
      scene with
      materials = Array.append scene.materials materials;
      vbuffer = vbuffer';
      tbuffer = Array.append scene.tbuffer tbuffer;
      nbuffer = Array.append scene.nbuffer nbuffer;
      bbuffer = Array.append scene.bbuffer bbuffer;
      bbox = Bih.Aabb.join scene.bbox object_bbox;
    }
  in
  (new_scene, object_bbox)

let update_bboxes scene =
  let vbuffer = scene.vbuffer in
  let bbuffer =
    Array.map
      (fun {t1; t2; t3; _} ->
        Misc.aabb_of_triangle vbuffer.(t1) vbuffer.(t2) vbuffer.(t3))
      scene.tbuffer
  in
  let object_bbox = Array.fold_left Bih.Aabb.join (Bih.Aabb.empty 3) bbuffer in
  {scene with bbuffer; bbox = object_bbox}

(* Translate all verties by [shift] *)
let translate_geometry scene shift =
  update_bboxes {scene with vbuffer = Array.map (V3.add shift) scene.vbuffer}

(* Apply a rotation matrix to all vertices in the scene. *)
let transform_geometry scene mat =
  let vbuffer = Array.map (V3.ltr mat) scene.vbuffer in
  let nbuffer =
    Array.map
      (fun {t1; t2; t3; _} ->
        V3.unit
          (V3.cross
             (V3.sub vbuffer.(t2) vbuffer.(t1))
             (V3.sub vbuffer.(t3) vbuffer.(t1))))
      scene.tbuffer
  in
  update_bboxes {scene with nbuffer; vbuffer}

(* Shifts all vertices so that the center of the object is (0, 0, 0).
 * This does not affect the lights. *)
let recenter scene =
  let open Bih.Aabb in
  let neg_center =
    Array.map2 ( +. ) scene.bbox.maxs scene.bbox.mins
    |> Array.map (( *. ) 0.5)
    |> (fun array -> V3.v array.(0) array.(1) array.(2))
    |> V3.neg
  in
  let scene =
    {scene with vbuffer = Array.map (V3.add neg_center) scene.vbuffer}
  in
  update_bboxes scene
