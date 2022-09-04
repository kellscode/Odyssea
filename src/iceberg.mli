(** Representation of a single iceberg *)

type t
(** The abstract type of values representing iceberg. *)

val tick : t -> int -> t
(** [tick t] is the iceberg after one game tick with new position and
    angle based on its velocities.*)

val iceberg_collisions : t list -> t list
(** [iceberg_collisions lst] detects if any icebergs in [lst] are
    currently colliding with each other (hitbox overlapping) and
    re-calculates appropriate properties (i.e. velocity).*)

val init_ice : float -> float -> float -> string -> t list -> t option
(** [init f x y id lst] is an iceberg with id [id] with initial position
    on the border of a rectangle with corners at (0,0) and ([x],[y]),
    initial velocity pointed towards the rectangle (middle of the
    screen), and not colliding with any icebergs in [lst]. [f] is a
    factor that multiplies the velocity.*)

val ice_pos : t -> float * float
(** [ice_pos t] is the position of [t].*)

val ice_angle : t -> float
(** [ice_angle t] is the angle of [t].*)

val ice_mom_angle : t -> float
(** [ice_mom_angle t] is the angle of the momentum of [t].*)

val ice_id : t -> string
(** [ice_id t] is the id of [t].*)

val ice_img_id : t -> string
(** [ice_img_id t] is the image id of [t].*)

val ice_size : t -> float
(** [ice_size] is the size of [t].*)

val ice_vel : t -> float
(** [ice_vel t] is the magnitude of the velocity of [t]. *)

val contact : t -> t -> bool
(**[contact i1 i2] is true if icebergs [i1] and [i2] are touching. Used
   for testing.*)
