(* axis-aligned bounding boxes *)
type t =
  {
    mins : float array;
    maxs : float array
  }

let float_min (x : float) (y : float) =
  if x < y then
    x
  else
    y

let float_max (x : float) (y : float) =
  if x < y then
    y
  else
    x

let array_min (array1 : float array) (array2 : float array) =
  Array.map2 float_min array1 array2

let array_max (array1 : float array) (array2 : float array) =
  Array.map2 float_max array1 array2

let array_sub (array1 : float array) (array2 : float array) =
  Array.map2 (-.) array1 array2


let empty n =
  {
    mins = Array.make n max_float;
    maxs = Array.make n (~-. max_float);
  }

let join b1 b2 =
  {
    mins = array_min b1.mins b2.mins;
    maxs = array_max b1.maxs b2.maxs
  }

let extents { mins; maxs } = array_sub maxs mins

let copy { mins; maxs } =
  {
    mins = Array.copy mins;
    maxs = Array.copy maxs
  }

let mem pt { mins; maxs } =
  let acc = ref true in
  let idx = ref 0 in
  let len = Array.length pt in
  while !acc && !idx < len do
    let i = !idx in
    acc := !acc && mins.(i) <= pt.(i) && pt.(i) <= maxs.(i);
    incr idx
  done;
  !acc
