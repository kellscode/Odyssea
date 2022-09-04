open Float

type t = {
  hp : int;
  position : float * float;
  ship_angle : float;
  x_velocity : float;
  y_velocity : float;
  rot_velocity : float;
  friction : float;
  rot_friction : float;
  size : float;
  counter : int;
}
(** type t defines our ship type. *)

(** [init_ship x y] initializes a ship at the coordinates given. *)
let init_ship x y =
  {
    hp = 100;
    position = (x /. 2., y /. 2.);
    ship_angle = pi /. 2.;
    x_velocity = 0.;
    y_velocity = 0.;
    rot_velocity = 0.;
    friction = 0.95;
    rot_friction = 0.8;
    size = 20.;
    counter = 0;
  }

(** [new_hp t damage] removes the hp of ship [t] by [damage] and resets
    counter. *)
let new_hp (t : t) (damage : int) : t =
  { t with counter = 30; hp = t.hp - damage }

(** [tick s] takes a ship [s] and updates its state by one tick. *)
let tick (s : t) =
  {
    s with
    position =
      (fst s.position +. s.x_velocity, snd s.position +. s.y_velocity);
    ship_angle = s.ship_angle +. s.rot_velocity;
    x_velocity =
      (if abs s.x_velocity < 0.01 then 0.
      else s.x_velocity *. s.friction);
    y_velocity =
      (if abs s.y_velocity < 0.01 then 0.
      else s.y_velocity *. s.friction);
    rot_velocity =
      (if abs s.rot_velocity < 0.01 then 0.
      else s.rot_velocity *. s.rot_friction);
  }

(** [move keys s] updates the ship's based on the keys list given. *)
let move (keys : Command.t list) (s : t) =
  let rec rec_move k s =
    match k with
    | [] -> s
    | Command.Up :: t ->
        rec_move t
          {
            s with
            x_velocity = s.x_velocity +. (cos s.ship_angle *. 0.15);
            y_velocity = s.y_velocity +. (sin s.ship_angle *. 0.15);
          }
    | Command.Down :: t ->
        rec_move t
          {
            s with
            x_velocity = s.x_velocity -. (cos s.ship_angle *. 0.15);
            y_velocity = s.y_velocity -. (sin s.ship_angle *. 0.15);
          }
    | Command.Left :: t ->
        rec_move t { s with rot_velocity = s.rot_velocity -. 0.01 }
    | Command.Right :: t ->
        rec_move t { s with rot_velocity = s.rot_velocity +. 0.01 }
  in
  rec_move keys s

(** [ship_hp sp] is a helper function to return the ship's hp. *)
let ship_hp (sp : t) = sp.hp

(** [ship_pos sp] is a helper function to return the ship's posiiton. *)
let ship_pos (sp : t) = sp.position

(** [ship_angle sp] is a helper function to return the ship's angle. *)
let ship_angle (sp : t) = sp.ship_angle

(** [set_ship_pos sp] is a helper function to move the ship. *)
let set_ship_pos (sp : t) pos = { sp with position = pos }

(** [ship_size sp] is a helper function to return the ship's size. *)
let ship_size (sp : t) = sp.size

(** [get_counter sp] is a helper function to return the ship's counter. *)
let get_counter (sp : t) = sp.counter

(** [update_counter sp] is a helper function to update the ship's
    counter. *)
let update_counter (sp : t) = { sp with counter = sp.counter - 1 }

let set_ship_angle (sp : t) ang = { sp with ship_angle = ang }
