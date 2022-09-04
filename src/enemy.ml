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
  difficulty : string;
}
(** type t defines our enemy type. *)

let init_pos x y pos =
  let spawn_x = if fst pos < x /. 2. then x else 0. in
  let spawn_y = if snd pos < y /. 2. then y else 0. in
  (spawn_x, spawn_y)

(** [player_rel_pos t pos] returns a vector to the player from the
    enemy. *)
let player_rel_pos t pos =
  (fst pos -. fst t.position, snd pos -. snd t.position)

(** [angle_to_pos pos] returns the angle facing in the direction of the
    given vector. *)
let angle_to_pos pos = atan2 (snd pos) (fst pos)

(** [init_ship x y] initializes a ship at the coordinates given. *)
let init_enemy x y diff pos =
  let player_pos = init_pos x y pos in
  match diff with
  | "Easy" ->
      {
        hp = 50;
        position = player_pos;
        ship_angle = angle_to_pos player_pos;
        x_velocity = 0.;
        y_velocity = 0.;
        rot_velocity = 0.;
        friction = 0.70;
        rot_friction = 0.4;
        size = 20.;
        counter = 30;
        difficulty = diff;
      }
  | "Regular" ->
      {
        hp = 75;
        position = player_pos;
        ship_angle = angle_to_pos player_pos;
        x_velocity = 0.;
        y_velocity = 0.;
        rot_velocity = 0.;
        friction = 0.85;
        rot_friction = 0.6;
        size = 20.;
        counter = 30;
        difficulty = diff;
      }
  | "Advanced" ->
      {
        hp = 100;
        position = player_pos;
        ship_angle = angle_to_pos player_pos;
        x_velocity = 0.;
        y_velocity = 0.;
        rot_velocity = 0.;
        friction = 0.95;
        rot_friction = 0.8;
        size = 20.;
        counter = 30;
        difficulty = diff;
      }
  | _ ->
      {
        hp = 100;
        position = player_pos;
        ship_angle = angle_to_pos player_pos;
        x_velocity = 0.;
        y_velocity = 0.;
        rot_velocity = 0.;
        friction = 0.95;
        rot_friction = 0.8;
        size = 200.;
        counter = 30;
        difficulty = diff;
      }

(** [new_hp t damage] removes the hp of ship [t] by [damage] and resets
    counter. *)
let new_enemy_hp (t : t) (damage : int) : t =
  { t with counter = 30; hp = t.hp - damage }

(** [move keys s] updates the ship's based on the keys list given. *)
let ai_move (keys : Command.t list) (s : t) =
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

let ai_keystroke (t : t) pos : Command.t list =
  let player_pos = player_rel_pos t pos in
  let ideal_angle = angle_to_pos player_pos in
  Command.Up
  :: [
       (let diff = t.ship_angle -. ideal_angle in
        if diff > 0. then
          if diff < pi then Command.Left else Command.Right
        else if diff > pi *. -1. then Command.Right
        else Command.Left);
     ]

let tick (old_s : t) pos =
  let s = ai_move (ai_keystroke old_s pos) old_s in
  {
    s with
    position =
      (fst s.position +. s.x_velocity, snd s.position +. s.y_velocity);
    ship_angle =
      (if s.ship_angle +. s.rot_velocity > 2. *. pi then
       s.ship_angle +. s.rot_velocity -. (2. *. pi)
      else if s.ship_angle +. s.rot_velocity < -1. *. (2. *. pi) then
        s.ship_angle +. s.rot_velocity +. (2. *. pi)
      else s.ship_angle +. s.rot_velocity);
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

(** [ship_hp sp] is a helper function to return the ship's hp. *)
let enemy_ship_hp (sp : t) = sp.hp

(** [ship_pos sp] is a helper function to return the ship's posiiton. *)
let enemy_ship_pos (sp : t) = sp.position

(** [ship_angle sp] is a helper function to return the ship's angle. *)
let enemy_ship_angle (sp : t) = sp.ship_angle

(** [set_ship_pos sp] is a helper function to move the ship. *)
let set_enemy_ship_pos (sp : t) pos = { sp with position = pos }

(** [ship_size sp] is a helper function to return the ship's size. *)
let enemy_ship_size (sp : t) = sp.size

(** [get_counter sp] is a helper function to return the ship's counter. *)
let get_enemy_counter (sp : t) = sp.counter

(** [update_counter sp] is a helper function to update the ship's
    counter. *)
let update_enemy_counter (sp : t) = { sp with counter = sp.counter - 1 }
