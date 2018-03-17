
open Types

let epsilon = 0.0001

(* Moller-Trumbore ray-triangle intersection *)
let test ray p0 p1 p2 =
  let edge1 = Float3.sub p1 p0 in
  let edge2 = Float3.sub p2 p0 in
  let pvec = Float3.cross ray.normal edge2 in
  let det = Float3.dot edge1 pvec in
  if det > (~-. epsilon) && (det < epsilon) then
    None
  else
    let inv_det = 1.0 /. det in
    let tvec    = Float3.sub ray.origin p0 in
    let ucoord  = (Float3.dot tvec pvec) *. inv_det in
    if (ucoord < 0.0) || (ucoord > 1.0) then
      None
    else
      let qvec   = Float3.cross tvec edge1 in
      let vcoord = (Float3.dot ray.normal qvec) *. inv_det in
      if vcoord < 0.0 || ucoord +. vcoord > 1.0 then
	None
      else
        let hit_dist = (Float3.dot edge2 qvec) *. inv_det in
	Some { t = hit_dist;
               u = ucoord;
               v = vcoord }
