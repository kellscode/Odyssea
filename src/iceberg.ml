open Float
open Random

type t = {
  pos : float * float;
  angle : float;
  angle_eq : float -> float;
  velocity : float * float;
  id : string;
  image_id : string;
  size : float;
}
(** This is the custom types used of the icebergs. [pos] refers to the
    x-y coordinate position of the iceberg. [angle] refers to the angle
    the iceberg sprite is pointing (not the direction of travel).
    [angle_eq] is the unique equation used to determine the iceberg
    angle. [velocity] is the x-y vector of the iceberg's velocity. [id]
    is as an identifier for collisions unique to each iceberg.
    [image_id]) is used for what the iceberg looks like, multiple
    icebergs may have the same [image_id] at any one time. [size] is
    simply the size of the iceberg similar to a circle's radius. *)

type vector = {
  one : float;
  two : float;
}
(** This is a simple two dimensional vector type used to simply the
    physics equations used for collisions. *)

(** [sub_vec v1 v2] is simply the difference between vectors [v1] and
    [v2], returning a new vector of that difference. *)
let sub_vec v1 v2 = { one = v1.one -. v2.one; two = v1.two -. v2.two }

(** [dot_prod v1 v2] is simply the inner product of vectors [v1] and
    [v2], returning a scalar. *)
let dot_prod v1 v2 = (v1.one *. v2.one) +. (v1.two +. v2.two)

(** [vec_mag v1] is the magnitude of vector [v1]. *)
let vec_mag v1 = sqrt (pow v1.one 2. +. pow v1.two 2.)

(** vec_scalar [s v1] effectively multiplies vector [v1] by the scalar
    [s] and returns the new vector. *)
let vec_scalar s v1 = { one = v1.one *. s; two = v1.two *. s }

(**[rand_pos x y] is a (float * float) that represents a point uniformly
   randomly generated on the borders of a rectangle with corners at
   (0,0) and ([x], [y]) .*)
let rand_pos (x : float) (y : float) (size : float) =
  let rand_side = float ((2. *. x) +. (2. *. y)) in
  if rand_side <= x then (float x, -2. *. size)
  else if rand_side <= 2. *. x then (float x, y +. (size *. 2.))
  else if rand_side <= (2. *. x) +. y then (-2. *. size, float y)
  else (x +. (2. *. size), float y)

(** [rand_vel pos win_x win_y] is a (float * float) that represents a
    velocity of an object with position [pos], and velocity randomly
    generated such that its trajectory will cross a rectangle with
    corners at (0,0) and ([win_x], [win_y]).*)
let rand_vel pos win_x win_y size factor =
  let x, y = pos in
  let rand_x, rand_y = (float win_x, float win_y) in
  let rand_speed = (float 3. +. 1.) *. factor in
  let dist = sqrt (pow (x -. rand_x) 2. +. pow (y -. rand_y) 2.) in
  ( rand_speed *. (rand_x -. x) /. dist,
    rand_speed *. (rand_y -. y) /. dist )

(** [tick ice n] updates the state of one iceberg [ice] by one tick on
    tick [n]. *)
let tick (ice : t) (n : int) =
  {
    ice with
    pos =
      (fst ice.pos +. fst ice.velocity, snd ice.pos +. snd ice.velocity);
    angle = ice.angle_eq (float_of_int n);
  }

(** [ice_pos ice] a simple helper function that returns the position of
    [ice]. *)
let ice_pos (ice : t) = ice.pos

(** [ice_angle ice] a simple helper function that returns the angle of
    [ice]. *)
let ice_angle (ice : t) = ice.angle

(** [ice_mom_angle ice] a simple helper function that returns the angle
    of momentum. *)
let ice_mom_angle (ice : t) =
  atan2 (snd ice.velocity) (fst ice.velocity)

(** [ice_img_id ice] a simple helper function that returns the image id. *)
let ice_img_id (ice : t) = ice.image_id

(** [ice_id ice] a simple helper function that the id of [ice]. *)
let ice_id (ice : t) = ice.id

(** [ice_size ice] a simple helper function returns the size of [ice]. *)
let ice_size (ice : t) = ice.size

(** [ice_vel ice] a helper function that returns the magnitude of the
    velocity. *)
let ice_vel t1 =
  sqrt (pow (fst t1.velocity) 2. +. pow (snd t1.velocity) 2.)

(** [dist t1 t2] returns the distance between two icebergs [t1] and
    [t2]. *)
let dist t1 t2 =
  let x1, y1 = ice_pos t1 in
  let x2, y2 = ice_pos t2 in
  let dist =
    Float.sqrt (Float.pow (x1 -. x2) 2. +. Float.pow (y1 -. y2) 2.)
  in
  dist -. t1.size -. t2.size

(** [contact t1 t2] returns a bool on where [t1] and [t2] are colliding
    or not. *)
let contact t1 t2 =
  if t1.id = t2.id then false
  else if dist t1 t2 <= 0. then true
  else false

(** [calc_v t1 t2 t1m t2m mag1 mag2 contact_angle] returns the new
    velocty after collision for iceberg [t1] only. *)
let calc_v t1 t2 t1m t2m mag1 mag2 contact_angle =
  {
    one =
      ((mag1 *. cos (ice_mom_angle t1 -. contact_angle) *. (t1m -. t2m))
      +. (2. *. t2m *. mag2 *. cos (ice_mom_angle t2 -. contact_angle))
      )
      /. (t1m +. t2m) *. cos contact_angle
      +. mag1
         *. sin (ice_mom_angle t1 -. contact_angle)
         *. cos (contact_angle +. (pi /. 2.));
    two =
      ((mag1 *. cos (ice_mom_angle t1 -. contact_angle) *. (t1m -. t2m))
      +. (2. *. t2m *. mag2 *. cos (ice_mom_angle t2 -. contact_angle))
      )
      /. (t1m +. t2m) *. sin contact_angle
      +. mag1
         *. sin (ice_mom_angle t1 -. contact_angle)
         *. sin (contact_angle +. (pi /. 2.));
  }

(* Used to calculate new physics due to collisions. Collisions are
   elastic.*)
let real_physics t1 t2 =
  let t1m = pow t1.size 2. in
  let t2m = pow t2.size 2. in
  (* We define v and x vectors for both icebergs.*)
  let v1 = { one = fst t1.velocity; two = snd t1.velocity } in
  let v2 = { one = fst t2.velocity; two = snd t2.velocity } in
  let x1 = { one = fst t1.pos; two = snd t1.pos } in
  let x2 = { one = fst t2.pos; two = snd t2.pos } in
  let mag1 = vec_mag v1 in
  let mag2 = vec_mag v2 in
  (* let initial_energy = (t1m *. pow mag1 2.) +. (t2m *. pow mag2 2.)
     in *)
  (* Now we calculate the new velocity due to an elastic collision.*)
  (*let new_v1 = sub_vec v1 (vec_scalar (2. *. t2m /. (t1m +. t2m) *.
    (dot_prod (sub_vec v1 v2) (sub_vec x1 x2) /. (pow (vec_mag (sub_vec
    x2 x1))) 2.5)) (sub_vec x2 x1)) in*)
  let diff_pos = sub_vec x1 x2 in
  let contact_angle = atan2 diff_pos.two diff_pos.one in
  let new_v1 = calc_v t1 t2 t1m t2m mag1 mag2 contact_angle in
  (* let new_v2 = calc_v t2 t1 t2m t1m mag2 mag1 contact_angle in let
     new_energy = (t1m *. pow (vec_mag new_v1) 2.) +. (t2m *. pow
     (vec_mag new_v2) 2.) in let ratio = sqrt (initial_energy /.
     new_energy) in let new_v1 = vec_scalar 1. new_v1 in let new_v2 =
     vec_scalar 1. new_v2 in let new_energy = (t1m *. pow (vec_mag
     new_v1) 2.) +. (t2m *. pow (vec_mag new_v2) 2.) in (* Implementing
     conservation of energy. *) print_endline (Printf.sprintf "Initial
     energy:%f\tFinal energy%f\n" initial_energy new_energy); *)
  (* Now we calculate the new rotational velocity due to the
     collision. *)
  (*let new_rot = t1.rot_vel *. if contact_angle > t1.angle then ratio
    else -1. *. ratio in*)
  (* Finally we displace the icebergs away from each other to avoid
     contact. *)
  (*let new_x1 = { one = x1.one +. (abs (abs (x1.one -. x2.one) -.
    (t1.size +. t2.size)) /. 2.); two = x1.two +. (abs (abs (x1.two -.
    x2.two) -. (t1.size +. t2.size)) /. 2.); } in *)
  { t1 with velocity = (new_v1.one, new_v1.two) }

(** [sudo_physics t1 t2] is an alternative to the real_physics functions
    that uses simplified formulas for the physics calculations. *)
let sudo_physics t1 t2 =
  let t1m = pow t1.size 1. in
  let t2m = pow t2.size 1. in
  (* We define v and x vectors for both icebergs.*)
  let v1 = { one = fst t1.velocity; two = snd t1.velocity } in
  let v2 = { one = fst t2.velocity; two = snd t2.velocity } in
  let x1 = { one = fst t1.pos; two = snd t1.pos } in
  let x2 = { one = fst t2.pos; two = snd t2.pos } in
  let diff_pos = sub_vec x1 x2 in
  let contact_angle = atan (diff_pos.two /. diff_pos.one) in
  {
    t1 with
    velocity =
      ( (v1.one *. -1.)
        -. (v2.one *. (0.5 *. t2m /. (t1m +. t2m)) *. cos contact_angle),
        (v1.two *. -1.)
        -. (v2.two *. (0.5 *. t2m /. (t1m +. t2m)) *. sin contact_angle)
      );
  }

(* Returns an updated iceberg in that updates velocity for potential
   collisions with other icebergs in tList. *)
let rec colliding t tList =
  match tList with
  | [] -> t
  | head :: tail ->
      if contact t head then colliding (real_physics t head) tail
      else colliding t tail

(* This function works by going through every iceberg in the list and
   checking if it is colliding with any other. If it is, then it updates
   ITS OWN properties to reflect the collision. It works this way so as
   to allow collisions with multiple objects at once without issues
   (ideally).*)
let rec iceberg_collisions lst const_lst =
  (* First generate a list of collisions with the first iceberg*)
  (* This works by going through lst which contains all active icebergs.
     It then calls colliding on each iceberg, passing with it the
     const_lst which contains the states of the icebergs before
     collisions; colliding returns the iceberg with update physics. It
     then appends it to the recursive call on the rest of the
     icebergs.*)
  match lst with
  | [] -> []
  | x :: t -> colliding x const_lst :: iceberg_collisions t const_lst

let rec iceberg_collisions lst =
  let rec get_collisions acc = function
    | [] -> List.rev acc
    | h :: t ->
        let i_n, t_n =
          List.fold_left
            (fun x y ->
              if contact (fst x) y then
                ( real_physics (fst x) y,
                  snd x @ [ real_physics y (fst x) ] )
              else (fst x, snd x @ [ y ]))
            (h, []) t
        in
        get_collisions (i_n :: acc) t_n
  in
  get_collisions [] lst

(** [init_ice factor win_x win_y id others] is the initial constructor
    of an iceberg. *)
let rec init_ice
    (factor : float)
    (win_x : float)
    (win_y : float)
    (id : string)
    (others : t list) =
  let s = float 10. +. 15. in
  let p = rand_pos win_x win_y s in
  let ice =
    {
      pos = p;
      angle = float (2. *. pi);
      velocity = rand_vel p win_x win_y s factor;
      angle_eq =
        (let amplitude = 0.1 +. float 0.1 in
         let angle = float 2. *. pi in
         let speed = 10. +. float 10. in
         fun x -> amplitude *. sin ((x +. angle) /. speed));
      size = s;
      id;
      image_id = string_of_int (1 + Random.int 5);
    }
  in
  if List.exists (fun x -> contact x ice) others then None else Some ice
