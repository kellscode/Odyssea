(** Enemy controller and helper. *)

type t
(** The type [t] represents an enemy instance. It holds the position,
    velocity, acceleration, and health of the enemy. It also holds the
    angle the enemy is facing. *)

val tick : t -> float * float -> t
(** [tick t pos] is the enemy after one game tick, i.e. with new
    position and angle based on its old position [pos]*)

val init_enemy : float -> float -> string -> float * float -> t
(** [init_enemy x y str pos] creates an enemy instance. [x] and [y] are
    the screen size dimensions, [str] is the difficulty type, and [pos]
    is the position of the player.*)

val enemy_ship_pos : t -> float * float
(** [ship_pos ship] returns the position of the ship instance.*)

val enemy_ship_angle : t -> float
(** [ship_angle ship] returns the angle the ship is facing relative to
    the x-axis.*)

val enemy_ship_hp : t -> int
(** [ship_hp ship] returns the hp of the ship.*)

val enemy_ship_size : t -> float
(** [ship_size ship] returns the size of [ship].*)

val ai_move : Command.t list -> t -> t
(**[move lst t] adjusts the velocities of [t] according to the commands
   in [lst]*)

val set_enemy_ship_pos : t -> float * float -> t
(**[set__enemy_ship_pos t pos] is [t] with position at [pos]*)

val new_enemy_hp : t -> int -> t
(** [new_hp t damage] returns a new t with hp subtracted by damage*)

val get_enemy_counter : t -> int
(** [get_counter t] returns the counter of ship t. *)

val update_enemy_counter : t -> t
(** [update_counter t] returns the updated t with counter subtracted by
    1. *)
