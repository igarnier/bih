open Printf

module type EltType = 
sig

  type t

  val extents : t -> Aabb.t

end

module Make(E : EltType) =
struct

  type tri_index = int
  type dim       = int

  type node =
    | Leaf of { start : tri_index; stop : tri_index }
    | Node of { axis      : dim
              ; leftclip  : float
              ; rightclip : float
              ; left      : node
              ; right     : node }

  let index_of_max (array : float array) =
    if array.(0) > array.(1) then
      if array.(0) > array.(2)
      then 0
      else 2
    else
    if array.(1) > array.(2)
    then 1
    else 2

  type partition_outcome =
    | Trying
    | Stuck
    | RecurseRight
    | RecurseLeft
    | RecurseBoth

  let fmin (x : float) (y : float) =
    if x > y then y else x

  let fmax (x : float) (y : float) =
    if x > y then x else y

  let imin (x : int) (y : int) =
    if x > y then y else x

  let imax (x : int) (y : int) =
    if x > y then x else y

  let mindepth   = ref max_int
  let maxdepth   = ref 0
  let totaldepth = ref 0
  let leafcount  = ref 0

  let gather_stats depth =
    mindepth   := min !mindepth depth;
    maxdepth   := max !maxdepth depth;
    totaldepth := !totaldepth + depth;
    incr leafcount

  let exchange (index : int array) i j =
    let k = index.(i) in
    index.(i) <- index.(j);
    index.(j) <- k

  let sort_objects bboxes index half_dim dim left_obj right_obj =
    assert (left_obj < right_obj);
    let rec loop left_obj right_obj lclip rclip lmin rmax =
      if left_obj = right_obj then
        (left_obj, lclip, rclip, lmin, rmax)
      else
        (let left_box = bboxes.(index.(left_obj)) in
         let box_min  = left_box.Aabb.mins.(dim) in
         let box_max  = left_box.Aabb.maxs.(dim) in
         let middle   = (box_min +. box_max) *. 0.5 in
         if middle <= half_dim then
           loop (left_obj + 1) right_obj (fmax box_max lclip) rclip (fmin box_min lmin) (fmax box_max rmax)
         else
           (exchange index left_obj (right_obj-1);
            loop left_obj (right_obj - 1) lclip (fmin box_min rclip) (fmin box_min lmin) (fmax box_max rmax))
        )        
    in
    loop left_obj right_obj (~-. max_float) max_float max_float (~-. max_float)


  (* Middle-split, widest extent heuristic. Meaning of the arguments:
     . if there is only [leaf_bound] objects left to partition, we create a leaf
     . [objects] is an array of objects
     . [local_bbox] is the [Aabb.t] of the current set of objects
     . [index] is an array of integers, such that index.(i) represents the object [objects.(index.(i))]
     . The interval [obj_start, obj_end] corresponds to the indices of the objects we're partitioning
       (through [index]). The bounds are inclusive.
  *)
  let rec compute_bih leaf_bound objects bboxes local_bbox index start stop =
    if stop - start + 1 <= leaf_bound then
      Leaf { start; stop }
    else
      (* cut along the widest extent of the current bbox *)
      let extents   = Aabb.extents local_bbox in 
      let maxdim    = index_of_max extents in
      let rec continue dim =
        let half_dim = ((local_bbox.mins.(dim) +. local_bbox.maxs.(dim)) *. 0.5) in
        let left_end, lclip, rclip, lmin, rmax = sort_objects bboxes index half_dim dim start (stop+1) in
        if left_end = stop+1 then
          if rmax < half_dim then
            (let bbox = Aabb.copy local_bbox in
              bbox.Aabb.maxs.(dim) <- half_dim;
              compute_bih leaf_bound objects bboxes bbox index start stop)
          else
            let next = (dim+1) mod (Array.length extents) in
            if next = maxdim then
              Leaf { start; stop }
            else
              continue next
        else if left_end = start then
          if half_dim < lmin then
            (let bbox = Aabb.copy local_bbox in
             bbox.Aabb.mins.(dim) <- half_dim;
             compute_bih leaf_bound objects bboxes bbox index start stop)
          else 
            let next = (dim+1) mod (Array.length extents) in
            if next = maxdim then
              Leaf { start; stop }
            else
              continue next
        else
          let left_bbox = Aabb.copy local_bbox in
          left_bbox.Aabb.maxs.(dim) <- half_dim;
          let right_bbox = Aabb.copy local_bbox in
          right_bbox.Aabb.mins.(dim) <- half_dim;
          let left  = compute_bih leaf_bound objects bboxes left_bbox index start (left_end-1) in
          let right = compute_bih leaf_bound objects bboxes right_bbox index left_end stop in
          Node { axis = dim;
                 leftclip = lclip;
                 rightclip = rclip;
                 left;
                 right
               }
      in
      continue maxdim


end
