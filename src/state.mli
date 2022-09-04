(** Representation of the state of a game at one frame. *)

(** the abstract type representing the difficulty of the game*)
type level =
  | Easy
  | Regular
  | Advanced

type t
(** the abstract type representing the state of the game at a given time*)

val tick : int -> Command.t list -> t -> t
(** [tick x] runs the necessary processes to update the game state by
    one tick and renders the new state. Returns the new state. *)

val init_state : level -> float -> float -> int -> bool -> t
(** [init_state diff x y] creates an initial, default state instance. x
    and y are the screen size dimensions. num is the number of icebergs
    to initialize.*)

val ship_status : t -> float * float * float * int
(** [ship_pos t] is a tuple which represents the coordinate and the
    angle of the ship in [t], where the first float is the x coordinate,
    the second float is the y coordinate, and the third float is the
    angle. *)

val ices_pos : t -> (float * float * float) list
(** [ices_pos t] is a list of tuples which represents the coordinates
    and the angles of the iceberg in [t], where the first float is the x
    coordinate, the second float is the y coordinate, and the third
    float is the size. Used for testing.*)

val score : t -> int
(** [score st] is the player's score in state [st]*)

val icebergs : t -> Iceberg.t list
(** [icebergs st] is the list of all icebergs in [st]*)

val items : t -> Item.t list
(** [items st] is the list of all items in [st]*)

val enemies : t -> Enemy.t list
(** [enemies st] is the list of all enemies in [st]*)

val ship : t -> Ship.t
(** [score st] is ship in [st]*)

val diff : t -> level
(** [diff st] is the difficulty of [st]*)

val hit : t -> bool
(** [score st] is true if the ship in [st] is touching an iceberg, and
    false otherwise. *)

val set_ship_pos : t -> float * float -> t
(** [set_ship_pos st pos] is [st] with the ship at [pos]. Used for
    testing. *)

val set_ship_angle : t -> float -> t
(** [set_ship_pos st pos] is [st] with the ship at [pos]. Used for
    testing. *)

val collide : Ship.t -> Iceberg.t -> bool
(** [collide s i] is true if the ship [s] is touching iceberg [i] and
    false otherwise. *)

val diadem_ticks : t -> int
(** [diadem_ticks st] is the number of ticks the ship in [st] has
    remaining with the diadem ability. *)

val fire_ticks : t -> int
(** [diadem_ticks st] is the number of ticks the ship in [st] has
    remaining with the fire ability. *)

val lightning_ticks : t -> int
(** [diadem_ticks st] is the number of ticks the ship in [st] has
    remaining with the lightning ability. *)

val trident_ticks : t -> int
(** [diadem_ticks st] is the number of ticks the ship in [st] has
    remaining with the trident ability. *)
