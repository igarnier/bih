open Gg

(* Triangles index into a mesh (a vertex array), for sharing purposes *)
type triangle = {t1 : int; t2 : int; t3 : int; mat : int (* material *)}

(* Point light source *)
type light = {position : V3.t; intensity : float; color : V3.t}

(* Reflectivity properties of a surface. We use the Phong model. *)
type material = {
  m_color : V3.t;
  m_diffuse : float;
  (* Proportion of the light emitted by the actual light sources that is reflected by the surface *)
  m_specular : float;
  (* Specular componenet *)
  m_shininess : float; (* Shininess exponent *)
}

(* whenever a ray successfully hits a triangle, we store the point on the ray [t], and the point on the
 * triangle [(u,v)] (barycentric coords). This last element could be discarded for our purposes. *)
type hit = {
  t : float;
  (* intersection distance on the ray *)
  u : float;
  (* fst barycentric coord. *)
  v : float; (* snd barycentric coord. *)
}

(* a ray is a line going through [origin] and colinear to [normal] *)
type ray = {
  origin : V3.t;
  normal : V3.t;
  (* unit length vector *)
  inormal : V3.t; (* normal reciprocal *)
}
