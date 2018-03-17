(* 3x3 matrices *)
type t = float array

let zero = Array.make 9 0.0

let identity = 
  [| 1.0; 0.0; 0.0;
     0.0; 1.0; 0.0;
     0.0; 0.0; 1.0 |]

let transpose m = 
  [| m.(0); m.(3); m.(6);
     m.(1); m.(4); m.(7);
     m.(2); m.(5); m.(8) |] 

let apply_to_vec m v =
  let open Float3 in
  (* we could cheat for the x dim by passing directly m here - not critical though. *)
  (* it would be nice to avoid these array allocations too - FIXME. *)
  let x = dot v [| m.(0); m.(1); m.(2) |] in
  let y = dot v [| m.(3); m.(4); m.(5) |] in
  let z = dot v [| m.(6); m.(7); m.(8) |] in
  [| x; y; z |]

let rot angle v =
  let x = v.(0) in
  let y = v.(1) in
  let z = v.(2) in
  let c = cos angle in
  let s = sin angle in
  [| (x *. x *. (1. -. c) +. c)      ; (x *. y *. (1. -. c) -. z *. s) ; (x *. z *. (1. -. c) +. y *. s);
     (y *. x *. (1. -. c) +. z *. s) ; (y *. y *. (1. -. c) +. c)      ; (y *. z *. (1. -. c) -. x *. s);
     (x *. z *. (1. -. c) -. y *. s) ; (y *. z *. (1. -. c) +. x *. s) ; (z *. z *. (1. -. c) +. c);      |]     
