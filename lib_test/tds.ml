

(* let load_3ds : string -> int ->  obj =
      fun filename material_id ->
        let input  = open_in filename in
        let load_vertex () = 
          let coords2vertex x y z s t =
            ([| x; y; z |], (s, t))
          in
          (Scanf.fscanf input "%f %f %f %f %f\n" coords2vertex) in
        let load_face () = 
          Scanf.fscanf input "%d %d %d %d\n" (fun _ t1 t2 t3 ->  { t1; t2; t3; mat = material_id }) 
        in
        let vertex_num = Scanf.fscanf input "%d\n" (fun a -> a) in
        let vbuffer = Array.make vertex_num V3.zero  in
        for i = 1 to vertex_num do      
          let (point, ud) = load_vertex () in
          vbuffer.(i-1) <- point
        done;
        let triangle_num =  Scanf.fscanf input "%d\n" (fun a -> a) in
        let tbuffer = Array.make triangle_num { t1 = 0; t2 = 0; t3 = 0; mat = 0 } in
        for i = 1 to triangle_num do
          tbuffer.(i-1) <- (load_face ())
        done;
        close_in input;
        let obj = object_from_vertices_and_faces vbuffer tbuffer in
        (* print some info on the object before returning it *)
        let open Aabb in
        let open V3 in
        Printf.printf "loading %s:\n%d vertices\n%d triangles\n" filename (Array.length obj.vbuffer) (Array.length obj.tbuffer);
        let bbox = obj.bbox in
        Printf.printf "object bbox pos: %f %f %f to %f %f %f\n" bbox.mins.(0) bbox.mins.(1) bbox.mins.(2) bbox.maxs.(0) bbox.maxs.(1) bbox.maxs.(2);
        let diff = sub bbox.maxs bbox.mins in
        Printf.printf "object bbox extents: %f %f %f\n" diff.(0) diff.(1) diff.(2);
        obj *)
