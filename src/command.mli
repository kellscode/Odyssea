(**Reading keyboard input and translating it into commands.*)

(** The type [t] represents commands for a ship to move *)
type t =
  | Up
  | Down
  | Left
  | Right

val check_key : unit -> t list
(** [check_key ()] is a [t] list that contains [Up] if W is being
    pressed, [Down] if S is being pressed, [Left] if A is being pressed,
    and/or [Right] if D is being pressed.*)
