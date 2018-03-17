
type t = private float array

val zero : t

val identity : t

val transpose : t -> t

val apply_to_vec : t -> Float3.t -> Float3.t

val rot : float -> Float3.t -> t

