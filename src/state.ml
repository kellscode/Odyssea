open Iceberg
open Ship
open Float
open Item

type level =
  | Easy
  | Regular
  | Advanced

type t = {
  difficulty : level;
  num_icebergs : int;
  icebergs : Iceberg.t list;
  ship : Ship.t;
  items : Item.t list;
  enemies : Enemy.t list;
  window_x : float;
  window_y : float;
  score : int;
  hit : bool;
  wrap : bool;
  fire_ticks : int;
  trident_ticks : int;
  diadem_ticks : int;
  lightning_ticks : int;
}

let factor diff =
  match diff with
  | Easy -> 0.3
  | Regular -> 0.5
  | Advanced -> 1.5

(* returns a bool value whether item should be generated based on
   difficulty *)
let random_generate (diff : level) =
  let random_number =
    Random.int 10000000
    mod
    match diff with
    | Easy -> 1000
    | Regular -> 2000
    | Advanced -> 3000
  in
  if random_number = 1 then true else false

(* gen_item diff x y num displays items on screen. If the num (number of
   frame) is divisible by the random_int generated, then items appear on
   screen. *)
let rec gen_item (max_num : int) (st : t) =
  let list_items = st.items in
  let new_items =
    if random_generate st.difficulty then
      [ Item.init_item st.window_x st.window_y list_items ]
    else []
  in
  if max_num > 1 then
    gen_item (max_num - 1) { st with items = new_items @ list_items }
  else list_items @ new_items

(* determine how many items to create. *)
let num_items (st : t) =
  let max_num =
    match st.difficulty with
    | Easy -> 5 - List.length st.items
    | Regular -> 4 - List.length st.items
    | Advanced -> 3 - List.length st.items
  in
  { st with items = gen_item max_num st }

let rec gen_ice (diff : level) (x : float) (y : float) (num : int) =
  if num > 0 then
    let prev = gen_ice diff x y (num - 1) in
    match
      Iceberg.init_ice (factor diff) x y
        (prev |> List.length |> string_of_int)
        prev
    with
    | None -> prev
    | Some i -> i :: prev
  else []

let init_state diff x y num wrap =
  let i = gen_ice diff x y num in
  {
    difficulty = diff;
    num_icebergs = num;
    icebergs = i;
    ship = Ship.init_ship x y;
    items = [];
    enemies = [];
    window_x = x;
    window_y = y;
    score = 0;
    hit = false;
    wrap;
    fire_ticks = 0;
    trident_ticks = 0;
    lightning_ticks = 0;
    diadem_ticks = 0;
  }

let in_bounds st (ice : Iceberg.t) =
  let x, y = ice_pos ice in
  let size = ice_size ice in
  x <= st.window_x +. (2. *. size)
  && y <= st.window_y +. (2. *. size)
  && x >= -2. *. size
  && y >= -2. *. size

let in_bounds_items st (item : Item.t) =
  let x, y = item_pos item in
  let size = item_size item in
  x <= st.window_x +. (2. *. size)
  && y <= st.window_y +. (2. *. size)
  && x >= -2. *. size
  && y >= -2. *. size

let edge_detect st ship =
  let x, y = ship_pos ship in
  let s = ship_size ship in
  if st.wrap then
    set_ship_pos ship
      ( (if x > st.window_x +. s then -1. *. s
        else if x < -1. *. s then st.window_x +. s
        else x),
        if y > st.window_y +. s then -1. *. s
        else if y < -1. *. s then st.window_y +. s
        else y )
  else
    set_ship_pos ship
      ( max s (min (st.window_x -. s) x),
        max s (min (st.window_y -. s) y) )

let update_icebergs n st =
  let icebergs =
    st.icebergs |> iceberg_collisions
    |> List.filter (fun x -> in_bounds st x)
    |> fun y ->
    if st.trident_ticks = 0 then List.map (fun x -> Iceberg.tick x n) y
    else y
  in
  let num_icebergs_current = List.length icebergs in
  if st.fire_ticks > 0 then []
  else if num_icebergs_current < st.num_icebergs then
    match
      init_ice (factor st.difficulty) st.window_x st.window_y
        (string_of_int (n + st.num_icebergs))
        icebergs
    with
    | None -> icebergs
    | Some i -> i :: icebergs
  else icebergs

let update key n st =
  let icebergs = update_icebergs n st in
  {
    st with
    icebergs;
    ship =
      Ship.tick st.ship
      |> (fun x ->
           if st.lightning_ticks > 0 then Ship.tick x |> Ship.move key
           else x)
      |> Ship.move key |> edge_detect st;
    items =
      List.map
        (fun x -> Item.tick x)
        (List.filter (fun x -> Item.item_display_time x < 20.) st.items);
    enemies =
      List.map
        (fun x -> Enemy.tick x (Ship.ship_pos st.ship))
        (*List.filter (fun x -> Enemy.enemy_ship_hp x < 0)*) st.enemies;
    score =
      (if n mod 10 = 5 && ship_hp st.ship > 0 then st.score + 1
      else st.score);
    diadem_ticks =
      (if st.diadem_ticks > 0 then st.diadem_ticks - 1 else 0);
    lightning_ticks =
      (if st.lightning_ticks > 0 then st.lightning_ticks - 1 else 0);
    trident_ticks =
      (if st.trident_ticks > 0 then st.trident_ticks - 1 else 0);
    fire_ticks = (if st.fire_ticks > 0 then st.fire_ticks - 1 else 0);
  }

let ship_status st =
  let x, y = ship_pos st.ship in
  (x, y, ship_angle st.ship, ship_hp st.ship)

let score st = st.score
let icebergs st = st.icebergs
let items st = st.items
let enemies st = st.enemies
let ship st = st.ship
let hit st = st.hit
let diff st = st.difficulty

let ices_pos st =
  st.icebergs
  |> List.map (fun z ->
         let x, y = ice_pos z in
         (x, y, ice_size z))

let collide t1 t2 =
  let x1, y1 = ship_pos t1 in
  let x2, y2 = ice_pos t2 in
  let dist =
    Float.sqrt (Float.pow (x1 -. x2) 2. +. Float.pow (y1 -. y2) 2.)
  in
  if dist -. ship_size t1 -. ice_size t2 <= 0. then true else false

let collide_item t1 t2 =
  let x1, y1 = ship_pos t1 in
  let x2, y2 = item_pos t2 in
  let dist =
    Float.sqrt (Float.pow (x1 -. x2) 2. +. Float.pow (y1 -. y2) 2.)
  in
  if dist -. ship_size t1 -. item_size t2 <= 0. then true else false

let collide_enemy t1 t2 =
  let x1, y1 = ship_pos t1 in
  let x2, y2 = Enemy.enemy_ship_pos t2 in
  let dist =
    Float.sqrt (Float.pow (x1 -. x2) 2. +. Float.pow (y1 -. y2) 2.)
  in
  if dist -. ship_size t1 -. Enemy.enemy_ship_size t2 <= 0. then true
  else false

let rec collision t tList =
  match tList with
  | [] -> []
  | head :: tail ->
      if collide t head then head :: collision t tail
      else collision t tail

(* list of items collided with *)
let rec collision_items t itemList =
  match itemList with
  | [] -> []
  | head :: tail ->
      if collide_item t head then head :: collision_items t tail
      else collision_items t tail

let rec collision_enemies t enemyList =
  match enemyList with
  | [] -> []
  | head :: tail ->
      if collide_enemy t head then head :: collision_enemies t tail
      else collision_enemies t tail

let lose_life t =
  if
    Ship.get_counter t.ship <= 1
    && (collision t.ship t.icebergs <> []
       || collision_enemies t.ship t.enemies <> [])
  then
    let new_ship = Ship.new_hp t.ship 10 in
    { t with ship = new_ship; hit = true }
  else
    let new_ship = Ship.update_counter t.ship in
    { t with ship = new_ship; hit = false }

(* colliding with diadem makes ship become invincible for 5 sec.*)
let diadem_effect t = { t with diadem_ticks = 300 }

(* colliding with lighning makes ship become twice as fast for 5 sec.*)
let lightning_effect t = { t with lightning_ticks = 300 }

(* colliding with fire makes all icebergs disappear for 2 sec.*)
let fire_effect t = { t with fire_ticks = 120; icebergs = [] }

(* colliding with trident makes all icebergs freeze for 5 sec.*)
let trident_effect t = { t with trident_ticks = 300 }
let trident_ticks st = st.trident_ticks
let fire_ticks st = st.fire_ticks
let lightning_ticks st = st.lightning_ticks
let diadem_ticks st = st.diadem_ticks

(* removes item from list of items which are all items on screen*)
let rec remove_head items_list item =
  match items_list with
  | [] -> []
  | h :: t -> if h == item then t else h :: remove_head t item

(* item that ship most recently collided with will result in appropriate
   action. *)
let item_effect t =
  let items_collided = collision_items t.ship t.items in
  match items_collided with
  | [] -> t
  | head :: tail -> (
      match Item.item_string (Item.item_id head) with
      | "diadem" ->
          diadem_effect { t with items = remove_head t.items head }
      | "lightning" ->
          lightning_effect { t with items = remove_head t.items head }
      | "fire" ->
          fire_effect { t with items = remove_head t.items head }
      | "trident" ->
          trident_effect { t with items = remove_head t.items head }
      | _ -> failwith "there should be no other item type")

let get_item t =
  let item_lst = collision_items t.ship t.items in
  if item_lst <> [] then
    let new_ship = Ship.new_hp t.ship 10 in
    { t with ship = new_ship; hit = true }
  else t

let tick n keys st = st |> update keys n |> lose_life |> num_items

let set_ship_pos st pos =
  { st with ship = Ship.set_ship_pos st.ship pos }

let set_ship_angle st ang =
  { st with ship = Ship.set_ship_angle st.ship ang }

let spawn_enemy t =
  Enemy.init_enemy t.window_x t.window_y
    (match t.difficulty with
    | Easy -> "Easy"
    | Regular -> "Regular"
    | Advanced -> "Advanced")
    (Ship.ship_pos t.ship)

let enemy_state t =
  let enemy_lst = t.enemies in
  let new_enemies =
    match t.difficulty with
    | Easy -> begin
        match enemy_lst with
        | [] -> [ spawn_enemy t ]
        | _ -> enemy_lst
      end
    | Regular -> begin
        match enemy_lst with
        | [] -> [ spawn_enemy t ]
        | _ -> enemy_lst
      end
    | Advanced -> begin
        match enemy_lst with
        | [] -> [ spawn_enemy t ]
        | _ -> enemy_lst
      end
  in
  { t with enemies = new_enemies }

let tick n keys st =
  st |> update keys n
  |> (fun x -> if st.diadem_ticks = 0 then lose_life x else x)
  |> num_items |> enemy_state |> item_effect
