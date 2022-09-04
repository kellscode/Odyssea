(** Ship controller and helper. *)

type t
(** The type [t] represents a ship instance. It holds the position,
    velocity, acceleration, and health of the ship. It also holds the
    angle the ship is facing. *)

val tick : t -> t
(** [tick t] is the ship after one game tick, i.e. with new position and
    angle*)

val init_ship : float -> float -> t
(** [init_ship x y] creates an initial, default ship instance. x and y
    are the screen size dimensions.*)

val ship_pos : t -> float * float
(** [ship_pos ship] returns the position of the ship instance.*)

val ship_angle : t -> float
(** [ship_angle ship] returns the angle the ship is facing relative to
    the x-axis.*)

val ship_hp : t -> int
(** [ship_hp ship] returns the hp of the ship.*)

val ship_size : t -> float
(** [ship_size ship] returns the size of [ship].*)

val move : Command.t list -> t -> t
(**[move lst t] adjusts the velocities of [t] according to the commands
   in [lst]*)

val set_ship_pos : t -> float * float -> t
(**[set_ship_pos t pos] is [t] with position at [pos]*)

val set_ship_angle : t -> float -> t
(**[set_ship_angle t ang] is [t] with angle [ang]*)

val new_hp : t -> int -> t
(** [new_hp t damage] returns a new t with hp subtracted by damage*)

val get_counter : t -> int
(** [get_counter t] returns the counter of ship t. *)

val update_counter : t -> t
(** [update_counter t] returns the updated t with counter subtracted by
    1. *)
