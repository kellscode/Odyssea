open Float
open Random

type item_type =
  | Diadem
  | Lightning
  | Fire
  | Trident

type t = {
  pos : float * float;
  id : item_type;
  size : float;
  display_time : float;
}

let item_string (item : item_type) =
  match item with
  | Diadem -> "diadem"
  | Lightning -> "lightning"
  | Fire -> "fire"
  | Trident -> "trident"

(**[rand_pos x y] is a (float * float) that represents a point uniformly
   randomly generated on the borders of a rectangle with corners at
   ([0.1x], [0.1y]) and ([0.9x], [0.9y]) .*)
let rand_pos (x : float) (y : float) (size : float) =
  ((0.1 *. x) +. (0.8 *. float x), (0.1 *. y) +. (0.8 *. float y))

(* tick updates the item display_time by 1/60 seconds. Every tick, the
   display time is increased by 1/60. *)
let tick (item : t) =
  let time = item.display_time +. (1. /. 60.) in
  { item with display_time = time }

let item_pos (item : t) = item.pos
let item_id (item : t) = item.id
let item_size (item : t) = item.size
let item_display_time (item : t) = item.display_time

(* distance between one item and another. Should not overlap. *)
let dist t1 t2 =
  let x1, y1 = item_pos t1 in
  let x2, y2 = item_pos t2 in
  let dist =
    Float.sqrt (Float.pow (x1 -. x2) 2. +. Float.pow (y1 -. y2) 2.)
  in
  dist -. t1.size -. t2.size

(* whether two items are in contact. Should not happen. *)
let contact t1 t2 =
  if t1.id = t2.id then false
  else if dist t1 t2 <= 0. then true
  else false

let random_item item_num =
  match item_num with
  | 0 -> Diadem
  | 1 -> Lightning
  | 2 -> Fire
  | 3 -> Trident
  | _ -> Diadem

let rec init_item (win_x : float) (win_y : float) (others : t list) =
  let s = 20. in
  let p = rand_pos win_x win_y s in
  let item =
    {
      pos = p;
      size = s;
      id = random_item (Random.int 4);
      display_time = 0.0;
    }
  in
  if List.exists (fun x -> contact x item) others then
    init_item win_x win_y others
  else item