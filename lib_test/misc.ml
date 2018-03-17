
let aabb_of_triangle v1 v2 v3 =
  Bih.Aabb.(
    let mins =
      array_min v1 (array_min v2 v3)
    and maxs =
      array_max v1 (array_max v2 v3)
    in
    { mins; maxs }
  )
