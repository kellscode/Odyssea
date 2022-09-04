(** GUI controller. *)

val init : float -> float -> unit
(**[init x y] initializes a gui with a [x] [y] size screen.*)

val draw : State.t -> int -> int -> unit
(**[draw st n] draws the main game loop.*)

val draw_start :
  State.t -> int -> float * float -> (float * float) * bool
(**[draw_start st n pos] draws the starting animation. [pos] respresents
   the position of where to draw the ship. Returns a
   [(float * float) * bool], where the pair represents the position to
   draw the ship at the next tick, and the bool is [true] if the
   starting animation should continue next tick, and [false] otherwise.*)

val draw_end : State.t -> int -> int -> bool -> unit
(**[draw_end st n hs nh] draws the ending animation and menu. [hs] is
   the high_score, and [nh] is [true] if the current game is the high
   score, and false otherwise.*)
