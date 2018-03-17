(* vectors/points *)

(* we use an array instead of a record in order to project 
 * fields according to a given variable without branching. *)
type t = float array

let zero = [| 0.0; 0.0; 0.0 |]
let i    = [| 1.0; 0.0; 0.0 |]
let j    = [| 0.0; 1.0; 0.0 |]
let k    = [| 0.0; 0.0; 1.0 |]

let make x y z = [| x; y; z |]

let spread (x:float) = [| x; x; x |]

let add p1 p2 =
  let x = p1.(0) +. p2.(0) in
  let y = p1.(1) +. p2.(1) in
  let z = p1.(2) +. p2.(2) in
  [| x; y; z |]

let sub p1 p2 =
  let x = p1.(0) -. p2.(0) in
  let y = p1.(1) -. p2.(1) in
  let z = p1.(2) -. p2.(2) in
  [| x; y; z |]

let dot p1 p2 =
  p1.(0) *. p2.(0) +. p1.(1) *. p2.(1) +. p1.(2) *. p2.(2)

let cross p1 p2 =
  let x = p1.(1) *. p2.(2) -. p2.(1) *. p1.(2) in
  let y = p1.(2) *. p2.(0) -. p2.(2) *. p1.(0) in
  let z = p1.(0) *. p2.(1) -. p2.(0) *. p1.(1) in
  [| x; y; z |]

let mult p1 p2 =
  let x = p1.(0) *. p2.(0) in
  let y = p1.(1) *. p2.(1) in
  let z = p1.(2) *. p2.(2) in
  [| x; y; z |]

let length v =
  sqrt (dot v v)

let unit v =
  let il = 1.0 /. length v in
  [| v.(0) *. il;
     v.(1) *. il;
     v.(2) *. il |]

let scale v s = 
  [| v.(0) *. s;
     v.(1) *. s;
     v.(2) *. s |]

let neg v =
  scale v (~-. 1.0)

let near_equ epsilon p1 p2 =
  abs_float (p1.(0) -. p2.(0)) < epsilon &&
  abs_float (p1.(1) -. p2.(1)) < epsilon &&
  abs_float (p1.(2) -. p2.(2)) < epsilon

let print v =
  Printf.sprintf "{ %f; %f; %f }" v.(0) v.(1) v.(2)
