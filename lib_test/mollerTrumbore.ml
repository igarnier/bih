open Gg
open Types

let epsilon = 0.0001

(* Moller-Trumbore ray-triangle intersection *)
let test ray p0 p1 p2 =
  let edge1 = V3.sub p1 p0 in
  let edge2 = V3.sub p2 p0 in
  let pvec = V3.cross ray.normal edge2 in
  let det = V3.dot edge1 pvec in
  if det > ~-.epsilon && det < epsilon then None
  else
    let inv_det = 1.0 /. det in
    let tvec = V3.sub ray.origin p0 in
    let ucoord = V3.dot tvec pvec *. inv_det in
    if ucoord < 0.0 || ucoord > 1.0 then None
    else
      let qvec = V3.cross tvec edge1 in
      let vcoord = V3.dot ray.normal qvec *. inv_det in
      if vcoord < 0.0 || ucoord +. vcoord > 1.0 then None
      else
        let hit_dist = V3.dot edge2 qvec *. inv_det in
        Some {t = hit_dist; u = ucoord; v = vcoord}
