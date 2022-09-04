open Iceberg
open Ship
open Float
open Raylib
open State
open Item

(*represents data associated with an image*)
type img_data = {
  mutable source_rec : Rectangle.t;
  mutable texture : Texture.t;
  mutable w : float;
  mutable h : float;
}

(*represents data associated with ice bergs*)
type ice_data = {
  mutable img : img_data;
  mutable id : string;
}

(*represents all necessary image data*)
type data = {
  mutable ship_img : img_data option;
  mutable iceberg_data : ice_data list;
  mutable item_data : (string * img_data) list;
  mutable enemy_data : img_data list;
  mutable background_img : img_data option;
  mutable win_x : float;
  mutable win_y : float;
}

(*used for end animation*)
type end_status = {
  mutable angle : float;
  mutable size : float;
  mutable position : float * float;
  end_frame : int;
  mutable end_rec_y : float;
  mutable end_rec_yv : float;
}

(*represents gui relevant drawing parameters*)
type status = {
  mutable displayed_hp : float;
  mutable hit_frames : int;
  mutable offset : float * float;
  mutable e : end_status option;
  mutable paused_y : float;
  mutable ship_blur : Ship.t list;
}

(*initialize status*)
let s =
  {
    displayed_hp = 0.;
    hit_frames = 0;
    offset = (0., 0.);
    e = None;
    paused_y = -140.;
    ship_blur = [];
  }

(*initialize data*)
let d =
  {
    ship_img = None;
    background_img = None;
    item_data = [];
    enemy_data = [];
    iceberg_data = [];
    win_x = 0.;
    win_y = 0.;
  }

(**[load_img_data img] returns an [img_data] that contains the image
   data of [img]*)
let load_img_data img =
  let open Raylib.Rectangle in
  let open Raylib.Texture in
  let i = load_texture ("img/" ^ img) in
  let w = float (width i) in
  let h = float (height i) in
  { texture = i; source_rec = create 0. 0. w h; w; h }

(**[init win_x win_y] initializes the gui with a window width of [win_x]
   and a window height of [win_y]*)
let init win_x win_y =
  d.ship_img <- Some (load_img_data "ship.png");
  d.background_img <- Some (load_img_data "sea.png");
  d.enemy_data <-
    [ load_img_data "shark1.png"; load_img_data "shark2.png" ];
  d.win_x <- win_x;
  d.win_y <- win_y;
  s.e <-
    Some
      {
        angle = 90.;
        size = 0.;
        position = (win_x /. 2., 0.);
        end_frame = 0;
        end_rec_y = 0.;
        end_rec_yv = 0.;
      };
  let rec load_icebergs n =
    if n <= 5 then begin
      let i_d =
        {
          img = load_img_data ("iceberg" ^ string_of_int n ^ ".png");
          id = string_of_int n;
        }
      in
      d.iceberg_data <- i_d :: d.iceberg_data;
      load_icebergs (n + 1)
    end
  in
  load_icebergs 1;
  let rec load_items = function
    | [] -> ()
    | h :: t ->
        d.item_data <- (h, load_img_data (h ^ ".png")) :: d.item_data;
        load_items t
  in
  load_items [ "trident"; "diadem"; "fire"; "lightning" ]

(**[get_img_data img] is a four element tuple that represents the
   image's texture, source rectangle, width and height*)
let get_img_data img = (img.texture, img.source_rec, img.w, img.h)

(**[draw_icebergs bergs] draws all the icebergs in [bergs]*)
let rec draw_icebergs bergs =
  let open Raylib.Rectangle in
  match bergs with
  | head :: tail ->
      let x, y = ice_pos head in
      let x = x +. fst s.offset in
      let y = y +. snd s.offset in
      let size = ice_size head in
      let data =
        List.filter (fun x -> x.id = ice_img_id head) d.iceberg_data
        |> List.hd
      in
      let t, sr, w, h = get_img_data data.img in
      let dest_rect =
        if w > h then create x y (w /. h *. size *. 2.) (size *. 2.)
        else create x y (size *. 2.) (h /. w *. size *. 2.)
      in
      let center =
        Vector2.create (width dest_rect /. 2.) (height dest_rect /. 2.)
      in
      draw_texture_pro t sr dest_rect center
        (ice_angle head *. 180. /. pi)
        Color.white;
      draw_icebergs tail
  | _ -> ()

(**[draw_ship s a p alpha] draws the ship with size [s], angle [a] at
   position [p] with opacity [alpha]*)
let draw_ship size angle pos alpha =
  let open Raylib.Rectangle in
  let x, y = pos in
  let t, sr, w, h = get_img_data (Option.get d.ship_img) in
  let dest_rect =
    if w > h then create x y (w /. h *. size *. 2.) (size *. 2.)
    else create x y (size *. 2.) (h /. w *. size *. 2.)
  in
  let center =
    Vector2.create (width dest_rect /. 2.) (height dest_rect /. 2.)
  in
  draw_texture_pro t sr dest_rect center
    (angle *. 180. /. pi)
    (Color.create 255 255 255 alpha)

(**[draw_health h] draws the health bar with health [h]*)
let draw_health health =
  s.displayed_hp <- s.displayed_hp +. ((health -. s.displayed_hp) /. 8.);
  let hp = int_of_float (round s.displayed_hp) in
  if hp > 0 then
    draw_rectangle
      (20 + (150 * hp / 100))
      20
      (150 - (150 * hp / 100))
      25
      (Color.create 0 0 0
         (int_of_float (min 100. (100. -. (10. *. (10. -. float hp))))));
  draw_rectangle 20 20
    (150 * hp / 100)
    25
    (Color.create
       (255 - (255 * hp / 100))
       (255 * hp / 100)
       0
       (int_of_float (min 200. (200. -. (20. *. (10. -. float hp))))))

(**[draw_items i] draws all the items in [i]*)
let rec draw_items (items : Item.t list) =
  let open Raylib.Rectangle in
  match items with
  | h :: tail ->
      let x, y = item_pos h in
      let x = x +. fst s.offset in
      let y = y +. snd s.offset in
      let size = item_size h in
      let data = List.assoc (item_id h |> item_string) d.item_data in
      let t, sr, w, h = get_img_data data in
      let dest_rect =
        if w > h then create x y (w /. h *. size *. 2.) (size *. 2.)
        else create x y (size *. 2.) (h /. w *. size *. 2.)
      in
      let center =
        Vector2.create (width dest_rect /. 2.) (height dest_rect /. 2.)
      in
      draw_texture_pro t sr dest_rect center 0. Color.white;
      draw_items tail
  | _ -> ()

(**[draw_enemies e] draws all the enemies in [e]*)
let rec draw_enemies (enemies : Enemy.t list) n =
  let open Raylib.Rectangle in
  match enemies with
  | h :: tail ->
      let x, y = Enemy.enemy_ship_pos h in
      let size = Enemy.enemy_ship_size h in
      let angle = Enemy.enemy_ship_angle h in
      let t, sr, w, h =
        get_img_data (List.nth d.enemy_data (n mod 40 / 20))
      in
      let dest_rect =
        if w > h then create x y (w /. h *. size *. 2.) (size *. 2.)
        else create x y (size *. 2.) (h /. w *. size *. 2.)
      in
      let center =
        Vector2.create (width dest_rect /. 2.) (height dest_rect /. 2.)
      in
      draw_texture_pro t sr dest_rect center
        (angle *. 180. /. pi)
        (Color.create 0 0 0 200);
      draw_enemies tail n
  | _ -> ()

(**[draw_centered_text str s y c] draws string [str] with size [s] and
   color [c], centered on the x-axis and at a y value of [y].*)
let draw_centered_text str size y color =
  let width = measure_text str size in
  draw_text str ((int_of_float d.win_x - width) / 2) y size color

(**[draw_ship_blur] draws the ship blur in [st] if the player has the
   lightning ability*)
let draw_ship_blur st =
  if lightning_ticks st > 0 then (
    let open Ship in
    let i = ref 0 in
    while !i < 5 && !i < List.length s.ship_blur do
      let s = List.nth s.ship_blur !i in
      let s, a, p = (ship_size s, ship_angle s, ship_pos s) in
      draw_ship s a p (255 - (!i * 255 / 5));
      incr i
    done;
    s.ship_blur <- ship st :: s.ship_blur)
  else s.ship_blur <- []

(**[draw_all n s ice i h s a p] draws score [s], all the icebergs in
   [ice], all the items in [i], the health bar with health [h], the ship
   with size [s], angle [a] at position [p], and the background with a
   frame number of [n].*)
let draw_all st n score icebergs items enemies health size angle pos =
  let open Raylib.Rectangle in
  let t, sr, w, h = get_img_data (Option.get d.background_img) in
  let dest_rect =
    create
      (-20. +. fst s.offset)
      (-20. +. fst s.offset)
      (d.win_x +. 40. +. snd s.offset)
      (d.win_y +. 40. +. snd s.offset)
  in
  let center =
    Vector2.create
      (15. *. cos (float_of_int n /. 40.))
      (15. *. sin (float_of_int n /. 60.))
  in
  draw_texture_pro t sr dest_rect center 0. Color.white;
  draw_centered_text
    (Printf.sprintf "Score: %i" score)
    100
    ((int_of_float d.win_y / 2) - 50)
    (Color.create 0 0 0 40);
  draw_enemies enemies n;
  draw_icebergs icebergs;
  draw_items items;
  if health > 0. then draw_ship_blur st;
  let alpha =
    let d = diadem_ticks st in
    if d > 260 then 255 - (155 * (300 - d) / 40)
    else if d > 40 then 100
    else if d > 0 then 100 + (155 * (40 - d) / 40)
    else 255
  in
  draw_ship size angle pos alpha;
  draw_health health

(**[draw_grame st n] draws state [st] with a frame number of [n]*)
let draw_game st n =
  clear_background Color.darkblue;
  if hit st then s.hit_frames <- 30;
  if st |> ship |> ship_hp <= 0 then
    s.e <-
      Some
        {
          angle = st |> ship |> ship_angle;
          size = st |> ship |> ship_size;
          position = st |> ship |> ship_pos;
          end_frame = n;
          end_rec_y = -.d.win_y *. 0.6;
          end_rec_yv = 0.;
        };
  let ship = ship st in
  draw_all st n (score st) (icebergs st) (items st) (enemies st)
    (ship |> ship_hp |> float)
    (ship_size ship) (ship_angle ship)
    ( fst s.offset +. fst (ship_pos ship),
      snd s.offset +. snd (ship_pos ship) );
  let f = fire_ticks st in
  if f > 80 then
    draw_rectangle 0 0 (int_of_float d.win_x) (int_of_float d.win_y)
      (Color.create 240 240 240 (255 - (255 * (120 - f) / 40)));
  let t = trident_ticks st in
  if t > 260 then
    draw_rectangle 0 0 (int_of_float d.win_x) (int_of_float d.win_y)
      (Color.create 76 201 240 (255 - (255 * (300 - t) / 40)))

(**[hit_shake ()] modifies [s.offset] to shake the screen.*)
let hit_shake () =
  if s.hit_frames > 0 then begin
    s.hit_frames <- s.hit_frames - 1;
    s.offset <- (-5. +. Random.float 10., -5. +. Random.float 10.)
  end
  else s.offset <- (0., 0.)

let draw st n paused =
  hit_shake ();
  begin_drawing ();
  draw_game st n;
  draw_centered_text "paused" 120
    (int_of_float s.paused_y)
    (Color.create 0 0 0 255);
  if paused <> 0 then
    s.paused_y <-
      s.paused_y +. (((d.win_y /. 2.) -. 60. -. s.paused_y) /. 8.)
  else if s.paused_y > d.win_y then s.paused_y <- -140.
  else if s.paused_y > -120. then
    s.paused_y <- s.paused_y +. ((d.win_y +. 20. -. s.paused_y) /. 8.);
  end_drawing ()

let draw_start st n pos =
  hit_shake ();
  begin_drawing ();
  clear_background Color.darkblue;
  let size = st |> ship |> ship_size in
  draw_all st n 0 [] [] [] 0. size (pi /. 2.) pos;
  end_drawing ();
  ( (fst pos, snd pos +. (((d.win_y /. 2.) -. snd pos) /. 8.)),
    abs ((d.win_y /. 2.) -. snd pos) > 1. )

(**[draw_menu e st hs nh] draws the menu at the position specified by
   the end_status [e]*)
let draw_menu e st high_score new_high =
  e.end_rec_yv <-
    100.
    *. round
         ((e.end_rec_yv *. 0.8)
         +. (((0.2 *. d.win_y) -. e.end_rec_y) *. 0.03))
    /. 100.;
  e.end_rec_y <- 100. *. round (e.end_rec_y +. e.end_rec_yv) /. 100.;
  let end_rec =
    Rectangle.create (d.win_x *. 0.2) e.end_rec_y (d.win_x *. 0.6)
      (d.win_y *. 0.6)
  in
  draw_rectangle_rounded end_rec 0.2 10 (Color.create 255 255 255 200);
  draw_centered_text "game over"
    (int_of_float (d.win_y *. 0.15))
    (int_of_float (e.end_rec_y +. (d.win_y *. 0.02)))
    (Color.create 0 0 0 255);
  draw_centered_text
    (Printf.sprintf "score: %i" (score st))
    (int_of_float d.win_y / 10)
    (int_of_float (e.end_rec_y +. (d.win_y *. 0.19)))
    (Color.create 0 0 0 255);
  if new_high then
    draw_centered_text "new high score!"
      (int_of_float d.win_y / 10)
      (int_of_float (e.end_rec_y +. (d.win_y *. 0.31)))
      (Color.create 228 255 26 255)
  else
    draw_centered_text
      (Printf.sprintf "high score: %i" high_score)
      (int_of_float d.win_y / 10)
      (int_of_float (e.end_rec_y +. (d.win_y *. 0.31)))
      (Color.create 0 0 0 255);
  draw_centered_text "space to restart"
    (int_of_float d.win_y / 10)
    (int_of_float (e.end_rec_y +. (d.win_y *. 0.43)))
    (Color.create 0 0 0 255)

let draw_end st n high_score new_high =
  hit_shake ();
  begin_drawing ();
  clear_background Color.darkblue;
  let e = Option.get s.e in
  e.size <- e.size +. (-.e.size /. 20.);
  e.angle <- e.angle +. 0.2;
  draw_all st n (score st) (icebergs st) (items st) (enemies st) 0.
    e.size e.angle
    (fst s.offset +. fst e.position, snd s.offset +. snd e.position);
  if n - e.end_frame > 30 then draw_menu e st high_score new_high;
  end_drawing ()
