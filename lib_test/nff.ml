
(* Load neutral file format (NFF) *)
let load_nff :
  filename:string -> scale:float -> position:V3.t -> rotation:M3.t -> scene:t -> t =
  fun ~filename ~scale ~position ~rotation ~scene ->
    let fd = open_in filename in
    let vertices  = ref [] in
    let triangles = ref [] in
    let materials = ref [] in
    let vcount    = Array.length scene.vbuffer in
    let mcount    = Array.length scene.materials - 1 in
    let vertex_i   = ref vcount
    and material_i = ref mcount in
    begin
      try
        while true do
          let line = input_line fd in
          if String.length line = 0 then
            ()
          else if line.[0] = 'f' then
            (* material *)
            Scanf.sscanf (remove_head line) " %f %f %f %f %f %f " (fun x y z kd ks shine -> 
                incr material_i;
                let mat = {
                  m_color     = [| x; y; z |];
                  m_diffuse   = kd;
                  m_specular  = ks;
                  m_shininess = shine;
                } in
                materials := mat :: !materials
              )
          else if line.[0] = 'p' then
            (* announcing polygon - in our case, triangle only *)
            Scanf.sscanf (remove_head line) " %d " (fun d -> if d <> 3 then raise LoadError else ())
          else
            (* if no f nor p then three vertices *)
            (let v1 = Scanf.sscanf line " %f %f %f " (fun x y z -> 
                 let v = [| x ; y; z |] in
                 (V3.add (M3.apply_to_vec rotation (V3.scale v scale)) position)
               ) in
             let line = input_line fd in
             let v2 = Scanf.sscanf line " %f %f %f " (fun x y z -> 
                 let v = [| x ; y; z |] in
                 (V3.add (M3.apply_to_vec rotation (V3.scale v scale)) position)
               ) in
             let line = input_line fd in
             let v3 = Scanf.sscanf line " %f %f %f " (fun x y z -> 
                 let v = [| x ; y; z |] in
                 (V3.add (M3.apply_to_vec rotation (V3.scale v scale)) position)
               ) in
             let tri = { t1  = !vertex_i; 
                         t2  = !vertex_i + 1;
                         t3  = !vertex_i + 2;
                         mat = !material_i } in
             vertex_i := !vertex_i + 3;
             triangles := tri :: !triangles;
             vertices := v3 :: v2 :: v1 :: !vertices)
        done
      with
      | End_of_file -> close_in fd
    end;
    let vbuffer   = Array.of_list (List.rev !vertices) in
    let tbuffer   = Array.of_list !triangles in
    let materials = Array.of_list (List.rev !materials) in
    let (scene, bbox) = add_object_to_scene scene vbuffer tbuffer materials in
    (* print some info on the object before returning it *)
    let open Aabb in
    let open V3 in
    Printf.printf "loading %s:\n%d vertices\n%d triangles\n" filename (Array.length vbuffer) (Array.length tbuffer);
    Printf.printf "object bbox pos: %f %f %f to %f %f %f\n" bbox.mins.(0) bbox.mins.(1) bbox.mins.(2) bbox.maxs.(0) bbox.maxs.(1) bbox.maxs.(2);
    let diff = sub bbox.maxs bbox.mins in
    Printf.printf "object bbox extents: %f %f %f\n" diff.(0) diff.(1) diff.(2);
    scene
