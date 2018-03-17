open Angstrom

open Types
open Scene

type result =
  | Vertex of float * float * float
  | Face of int * int * int

(* this combinator is a bit lax *)
let float =
  take_while1 (function '0' .. '9' | '-' | '.' -> true | _ -> false) >>| float_of_string

let int =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

let spaces =
  skip_many (char ' ')

let vertex =
  (char 'v') >>= (fun _ ->
      spaces >>= (fun _ ->
          (sep_by spaces float) >>= (fun fl ->
              match fl with
              | [ x; y; z ] -> 
                return (Some (Vertex(x, y, z)))
              | _ -> failwith "vertex: error"
            )
        )
    )

let texture_coord =
  (string "vt") >>= (fun _ ->
      spaces >>= (fun _ ->
          sep_by spaces float
        )
    )

let triangle =
  (char 'f') >>= (fun _ ->
      spaces >>= (fun _ ->
          (sep_by spaces int) >>= (fun il ->
              match il with
              | [ x; y; z ] -> 
                return (Some (Face(x, y, z)))
              | _ -> failwith "vertex: error"
            )
        )
    )

let comment =
  (char '#') >>= (fun _ -> return None)

let mtllib =
  (string "mtllib") >>= (fun _ -> return None)

let usemtl =
  (string "usemtl") >>= (fun _ -> return None)

let group =
  (char 'g') >>= (fun _ -> return None)

let wavefront_line =
  choice [vertex; triangle; comment; mtllib; usemtl; group]

let load :
  filename:string -> scale:float -> material:int -> position:Float3.t -> rotation:Mat3.t -> scene:Scene.t -> t =
  fun ~filename ~scale ~material ~position ~rotation ~scene ->
    let fd = open_in filename in
    let vertices  = ref [] in
    let triangles = ref [] in
    let vcount = Array.length scene.vbuffer in
    begin
      try
        while true do
          let line = input_line fd in
          match parse_string wavefront_line line with
          | Result.Error _ | Result.Ok None -> ()
          | Result.Ok (Some (Vertex(x, y, z))) ->
            let v = Float3.make x y z in
            let v = Float3.add (Mat3.apply_to_vec rotation (Float3.scale v scale)) position in
            vertices := v :: !vertices
          | Result.Ok (Some (Face(i, j, k))) ->
            let tri = { t1 = i - 1 + vcount;
                        t2 = j - 1 + vcount;
                        t3 = k - 1 + vcount;
                        mat = material } in
            triangles := tri :: !triangles
        done
      with
      | End_of_file -> close_in fd
    end;
    let vbuffer = Array.of_list (List.rev !vertices) in
    let tbuffer = Array.of_list !triangles in
    let (scene, bbox) = add_object_to_scene scene vbuffer tbuffer [||] in
    (* print some info on the object before returning it *)
    let open Bih.Aabb in
    Printf.printf "loading %s:\n%d vertices\n%d triangles\n" filename (Array.length vbuffer) (Array.length tbuffer);
    Printf.printf "object bbox pos: %f %f %f to %f %f %f\n" bbox.mins.(0) bbox.mins.(1) bbox.mins.(2) bbox.maxs.(0) bbox.maxs.(1) bbox.maxs.(2);
    let diff = Array.map2 (fun x y -> x -. y) bbox.maxs bbox.mins in
    Printf.printf "object bbox extents: %f %f %f\n" diff.(0) diff.(1) diff.(2);
    scene



  
(* Remove first two characters of a line *)
let remove_head s =
  String.sub s 2 (String.length s - 2)

let count_spaces s =
  let c = ref 0 in
  for i = 1 to String.length s - 2 do
    let c1 = s.[i-1] in
    let c2 = s.[i] in
    let c3 = s.[i+1] in
    if c1 <> ' ' && c2 = ' ' && c3 <> ' ' then
      incr c
    else ()
  done;
  !c

(* Loading a slightly different form of wavefront files, as found in sponza.obj *)
let load_ext :
  filename:string -> scale:float -> material:int -> position:Float3.t -> rotation:Mat3.t -> scene:t -> t =
  fun ~filename ~scale ~material ~position ~rotation ~scene -> 
    let fd = open_in filename in
    let vertices  = ref [] in
    let triangles = ref [] in
    let vcount = Array.length scene.vbuffer in
    begin
      try
        while true do
          let line = input_line fd in
          if String.length line = 0 then ()
          else if line.[0] = 'v' then
            let v = Scanf.sscanf (remove_head line) " %f %f %f " (fun x y z ->
                let v = [| x ; y; z |] in
                (Float3.add (Mat3.apply_to_vec rotation (Float3.scale v scale)) position)
              ) in
            vertices := v :: !vertices
          else if line.[0] = 'f' then
            let data  = remove_head line in
            let spaces = count_spaces data in
            if spaces = 2 then
              let f = Scanf.sscanf (remove_head line) " %i/%i %i/%i %i/%i "
                  (fun t1 _ t2 _ t3 _ ->
                     { t1 = t1 - 1 + vcount;
                       t2 = t2 - 1 + vcount;
                       t3 = t3 - 1 + vcount;
                       mat = material 
                     }
                  ) in                
              triangles := f :: !triangles
            else if spaces = 3 then
              let f1, f2 = Scanf.sscanf (remove_head line) " %i/%i %i/%i %i/%i %i/%i "
                  (fun t1 _ t2 _ t3 _ t4 _ ->
                     let tri1 = {
                       t1 = t1 - 1 + vcount;
                       t2 = t2 - 1 + vcount;
                       t3 = t3 - 1 + vcount;
                       mat = material 
                     } in
                     let tri2 = {
                       t1 = t2 - 1 + vcount;
                       t2 = t3 - 1 + vcount;
                       t3 = t4 - 1 + vcount;
                       mat = material
                     } in
                     (tri1, tri2)                  
                  ) in
              triangles := f2 :: f1 :: !triangles
            else failwith "too much points in face"
          else ()
        done
      with
      | End_of_file -> close_in fd
    end;
    let vbuffer = Array.of_list (List.rev !vertices) in
    let tbuffer = Array.of_list !triangles in
    let (scene, bbox) = add_object_to_scene scene vbuffer tbuffer [||] in
    (* print some info on the object before returning it *)
    let open Bih.Aabb in
    Printf.printf "loading %s:\n%d vertices\n%d triangles\n" filename (Array.length vbuffer) (Array.length tbuffer);
    Printf.printf "object bbox pos: %f %f %f to %f %f %f\n" bbox.mins.(0) bbox.mins.(1) bbox.mins.(2) bbox.maxs.(0) bbox.maxs.(1) bbox.maxs.(2);
    let diff = Float3.sub bbox.maxs bbox.mins in
    Printf.printf "object bbox extents: %f %f %f\n" diff.(0) diff.(1) diff.(2);
    scene
