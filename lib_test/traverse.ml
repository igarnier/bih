open Types
open Gg

type tri_index = int

type outcome = NoHit | Hit of tri_index * hit

module Elt = struct
  type t = triangle

  type state = Scene.t

  let dim = 3

  let extents {Scene.vbuffer; _} {t1; t2; t3; _} =
    Misc.aabb_of_triangle vbuffer.(t1) vbuffer.(t2) vbuffer.(t3)
end

module Tree = Bih.Make (Elt)

(* Intersect a ray with all the triangles between [tri_start; tri_end]. Picks closest hit, if any. *)
let intersect_ray {Scene.vbuffer; tbuffer; nbuffer; _} tri_index ray tmin tmax
    tri_start tri_end =
  let min_hit = ref NoHit in
  for i = tri_start to tri_end do
    let {t1; t2; t3; _} = tbuffer.(tri_index.(i)) in
    if V3.dot ray.normal nbuffer.(tri_index.(i)) > 0.0 then ()
    else
      let v1 = vbuffer.(t1) and v2 = vbuffer.(t2) and v3 = vbuffer.(t3) in
      let res = MollerTrumbore.test ray v1 v2 v3 in
      match res with
      | Some ({t; _} as hit) -> (
        match !min_hit with
        | NoHit ->
            min_hit := Hit (tri_index.(i), hit)
        | Hit (_, {t = t'; _}) ->
            if t < t' && tmin <= t && t <= tmax then
              min_hit := Hit (tri_index.(i), hit)
            else () )
      | None ->
          ()
  done ;
  !min_hit

let fmin (x : float) (y : float) = if x < y then x else y

let fmax (x : float) (y : float) = if x < y then y else x

let proj v i =
  match i with 0 -> V3.x v | 1 -> V3.y v | 2 -> V3.z v | _ -> assert false
  [@@inline]

(* Ray traversal. Could be made simpler by symmetrising the code, at the cost of some alloc. *)
let rec traverse obj tri_index ray tmin tmax bih =
  if tmin >= tmax then NoHit
  else
    match bih with
    | Bih.Leaf {start; stop} ->
        intersect_ray obj tri_index ray tmin tmax start stop
    | Bih.Node {axis; leftclip; rightclip; left; right} ->
        (* TODO : dim <- axis *)
        if proj ray.normal axis >= 0.0 then
          (* going left-to-right : first left, then right *)
          let ray_start =
            proj ray.origin axis +. (proj ray.normal axis *. tmin)
          in
          let ray_end =
            proj ray.origin axis +. (proj ray.normal axis *. tmax)
          in
          if ray_start <= leftclip then
            (* going through left subtree *)
            let far_clip =
              fmin
                ((leftclip -. proj ray.origin axis) *. proj ray.inormal axis)
                tmax
            in
            let left_hit = traverse obj tri_index ray tmin far_clip left in
            if leftclip <= rightclip then
              match left_hit with
              | NoHit ->
                  if rightclip <= ray_end then
                    let near_clip =
                      fmax
                        ( (rightclip -. proj ray.origin axis)
                        *. proj ray.inormal axis )
                        tmin
                    in
                    traverse obj tri_index ray near_clip tmax right
                  else NoHit
              | Hit _ ->
                  left_hit (* early exit *)
            else if rightclip <= ray_end then
              let near_clip =
                fmax
                  ((rightclip -. proj ray.origin axis) *. proj ray.inormal axis)
                  tmin
              in
              let right_hit =
                traverse obj tri_index ray near_clip tmax right
              in
              match (left_hit, right_hit) with
              | (NoHit, NoHit) ->
                  NoHit
              | (NoHit, x) | (x, NoHit) ->
                  x
              | (Hit (_, {t = t1; _}), Hit (_, {t = t2; _})) ->
                  if t1 < t2 then left_hit else right_hit
            else left_hit
          else if rightclip <= ray_end then
            let near_clip =
              fmax
                ((rightclip -. proj ray.origin axis) *. proj ray.inormal axis)
                tmin
            in
            traverse obj tri_index ray near_clip tmax right
          else NoHit
        else
          (* going right-to-left : first right, then left *)
          let ray_start =
            proj ray.origin axis +. (proj ray.normal axis *. tmin)
          in
          let ray_end =
            proj ray.origin axis +. (proj ray.normal axis *. tmax)
          in
          if rightclip <= ray_start then
            (* going through right subtree *)
            let far_clip =
              fmin
                ((rightclip -. proj ray.origin axis) *. proj ray.inormal axis)
                tmax
            in
            let right_hit = traverse obj tri_index ray tmin far_clip right in
            if leftclip < rightclip then
              match right_hit with
              | NoHit ->
                  if ray_end <= leftclip then
                    let near_clip =
                      fmax
                        ( (leftclip -. proj ray.origin axis)
                        *. proj ray.inormal axis )
                        tmin
                    in
                    traverse obj tri_index ray near_clip tmax left
                  else NoHit
              | Hit _ ->
                  right_hit (* early exit *)
            else if ray_end <= leftclip then
              let near_clip =
                fmax
                  ((leftclip -. proj ray.origin axis) *. proj ray.inormal axis)
                  tmin
              in
              let left_hit = traverse obj tri_index ray near_clip tmax left in
              match (right_hit, left_hit) with
              | (NoHit, NoHit) ->
                  NoHit
              | (NoHit, x) | (x, NoHit) ->
                  x
              | (Hit (_, {t = t1; _}), Hit (_, {t = t2; _})) ->
                  if t1 < t2 then right_hit else left_hit
            else right_hit
          else if ray_end <= leftclip then
            let near_clip =
              fmax
                ((leftclip -. proj ray.origin axis) *. proj ray.inormal axis)
                tmin
            in
            traverse obj tri_index ray near_clip tmax left
          else NoHit

(* Seek only first hit for shadow rays, instead of closest one. *)
let intersect_ray_shadow {Scene.vbuffer; tbuffer; _} tri_index ray tri_start
    tri_end =
  let nohit = ref true in
  let i = ref tri_start in
  while !nohit && !i <= tri_end do
    let {t1; t2; t3; _} = tbuffer.(tri_index.(!i)) in
    (*if Float3.dot ray.normal nbuffer.(tri_index.(i)) > 0.0 then
      ()
      else*)
    let v1 = vbuffer.(t1) and v2 = vbuffer.(t2) and v3 = vbuffer.(t3) in
    let res = MollerTrumbore.test ray v1 v2 v3 in
    (match res with Some _ -> nohit := false | None -> ()) ;
    incr i
  done ;
  not !nohit

(* Traversal specialized for shadow rays - again, we do not care at finding the closest hit here. *)
let rec traverse_shadow obj tri_index ray tmin tmax bih =
  if tmin >= tmax then false
  else
    match bih with
    | Bih.Leaf {start; stop} ->
        intersect_ray_shadow obj tri_index ray start stop
    | Node {axis; leftclip; rightclip; left; right} ->
        if proj ray.normal axis >= 0.0 then
          (* going left-to-right : first left, then right *)
          let ray_start =
            proj ray.origin axis +. (proj ray.normal axis *. tmin)
          in
          let ray_end =
            proj ray.origin axis +. (proj ray.normal axis *. tmax)
          in
          if ray_start <= leftclip then
            (* going through left subtree *)
            let far_clip =
              fmin
                ((leftclip -. proj ray.origin axis) *. proj ray.inormal axis)
                tmax
            in
            let left_hit =
              traverse_shadow obj tri_index ray tmin far_clip left
            in
            if left_hit then true
            else if leftclip <= rightclip then
              if rightclip <= ray_end then
                let near_clip =
                  fmax
                    ( (rightclip -. proj ray.origin axis)
                    *. proj ray.inormal axis )
                    tmin
                in
                traverse_shadow obj tri_index ray near_clip tmax right
              else false
            else if rightclip <= ray_end then
              let near_clip =
                fmax
                  ((rightclip -. proj ray.origin axis) *. proj ray.inormal axis)
                  tmin
              in
              traverse_shadow obj tri_index ray near_clip tmax right
            else false
          else if rightclip <= ray_end then
            let near_clip =
              fmax
                ((rightclip -. proj ray.origin axis) *. proj ray.inormal axis)
                tmin
            in
            traverse_shadow obj tri_index ray near_clip tmax right
          else false
        else
          (* going right-to-left : first right, then left *)
          let ray_start =
            proj ray.origin axis +. (proj ray.normal axis *. tmin)
          in
          let ray_end =
            proj ray.origin axis +. (proj ray.normal axis *. tmax)
          in
          if rightclip <= ray_start then
            (* going through right subtree *)
            let far_clip =
              fmin
                ((rightclip -. proj ray.origin axis) *. proj ray.inormal axis)
                tmax
            in
            let right_hit =
              traverse_shadow obj tri_index ray tmin far_clip right
            in
            if right_hit then true
            else if leftclip < rightclip then
              if ray_end <= leftclip then
                let near_clip =
                  fmax
                    ( (leftclip -. proj ray.origin axis)
                    *. proj ray.inormal axis )
                    tmin
                in
                traverse_shadow obj tri_index ray near_clip tmax left
              else false
            else if ray_end <= leftclip then
              let near_clip =
                fmax
                  ((leftclip -. proj ray.origin axis) *. proj ray.inormal axis)
                  tmin
              in
              traverse_shadow obj tri_index ray near_clip tmax left
            else false
          else if ray_end <= leftclip then
            let near_clip =
              fmax
                ((leftclip -. proj ray.origin axis) *. proj ray.inormal axis)
                tmin
            in
            traverse_shadow obj tri_index ray near_clip tmax left
          else false
