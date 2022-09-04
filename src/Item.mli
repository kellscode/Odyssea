(** Representation of a single item *)

type item_type
(** The type representing the item type.*)

type t
(** The abstract type of values representing item. *)

val tick : t -> t
(** [tick t] is the iceberg after one game tick, i.e. with new size *)

val item_string : item_type -> string
(** [item_string t] is the string of the item type of [t]*)

val init_item : float -> float -> t list -> t
(** [init x y lst] is a random item with initial position inside a
    rectangle with corners at ([0.1x], [0.1y]) and ([0.9x], [0.9y]), and
    not touching any other items in [lst].*)

val item_pos : t -> float * float
(** [item_pos t] is the position of [t].*)

val item_id : t -> item_type
(** [item_id t] is the id of [t].*)

val item_size : t -> float
(** [item_size] is the size of [t].*)

val item_display_time : t -> float
(** [item_display_time] is time [t] has existed.*)
