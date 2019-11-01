open Gg
module V = V3

let to_array v = [|V3.x v; V3.y v; V3.z v|]

let aabb_of_triangle v1 v2 v3 =
  let v1 = to_array v1 in
  let v2 = to_array v2 in
  let v3 = to_array v3 in
  Bih.Aabb.(
    let mins = array_min v1 (array_min v2 v3)
    and maxs = array_max v1 (array_max v2 v3) in
    {mins; maxs})
