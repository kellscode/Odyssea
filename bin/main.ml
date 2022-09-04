open Game
open Iceberg
open Command
open State
open Fancyprint
open Raylib
open Gui

(** [add_score sc file diff] appends score [sc] with difficulty [diff]
    to file [file]*)
let add_score sc file diff =
  let chan =
    open_out_gen
      [ Open_creat; Open_text; Open_append ]
      0o640 "bin/scores.txt"
  in
  let d =
    match diff with
    | Easy -> "E"
    | Regular -> "I"
    | Advanced -> "H"
  in
  Printf.fprintf chan "%s%i\n" d sc;
  close_out chan

(** [read_scores file diff] is an int list of the scores with difficulty
    [diff] in [file]*)
let read_scores file diff =
  let chan = open_in file in
  let d =
    match diff with
    | Easy -> 'E'
    | Regular -> 'I'
    | Advanced -> 'H'
  in
  let rec read_rec file data =
    try
      let line = input_line chan in
      if String.get line 0 = d then
        (String.sub line 1 (String.length line - 1) |> int_of_string)
        :: data
        |> read_rec file
      else read_rec file data
    with
    | End_of_file -> data
    | _ -> failwith "failed to read data"
  in
  read_rec file []

(** [game_end st n scores] is the game loop after the player has lost.*)
let rec game_end st n high_score new_high =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let next_st = tick n [] st in
      draw_end st n high_score new_high;
      if is_key_down Key.Space then
        game_start
          (init_state (diff st)
             (float (get_screen_width ()))
             (float (get_screen_height ()))
             30 false)
          0
          (float (get_screen_width ()) /. 2., 0.)
          true
      else game_end next_st (n + 1) high_score new_high

(** [game_loop st n] is main game loop.*)

and game_loop st n paused =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      draw st n paused;
      let hp = st |> ship |> Ship.ship_hp in
      let paused =
        if is_key_pressed Key.Space then
          if paused = 0 then 16 else paused - 1
        else if paused = 1 then 1
        else if paused = 0 then 0
        else paused - 1
      in
      if hp > 0 then
        if paused = 0 then
          let next_st = tick n (check_key ()) st in
          game_loop next_st (n + 1) paused
        else game_loop st n paused
      else
        let next_st = tick n [] st in
        add_score (score st) "bin/scores.txt" (diff st);
        let scores = read_scores "bin/scores.txt" (diff st) in
        let high_score, new_high =
          List.fold_right
            (fun y x ->
              let hs, nh = x in
              if y > hs then if y = score st then (y, true) else (y, nh)
              else x)
            scores (0, false)
        in

        game_end next_st (n + 1) high_score new_high

(** [game_loop st n pos continue] is the start animation loop. [pos] is
    the position of the ship, and [continue] is [true] if the start
    animation has not finished, and [false] if the start animation has
    finished.*)
and game_start st n pos continue =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let next_st = tick n [] st in
      if continue then
        let pos, continue = draw_start st n pos in
        game_start st (n + 1) pos continue
      else game_loop next_st (n + 1) 0

(** [init_game diff win_x win_y] initializes a *)
let init_game diff win_x win_y =
  Raylib.set_target_fps 60;
  Gui.init (float win_x) (float win_y);
  game_start
    (init_state diff (float win_x) (float win_y) 30 false)
    0
    (float win_x /. 2., 0.)
    true

let rec menu () =
  print_endline
    "\n〰〰〰MENU〰〰〰\n[1] Tutorial\n[2] Start game\n[3] Quit\n\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | input -> (
      match input with
      | "1" ->
          ANSITerminal.erase Screen;
          ANSITerminal.set_cursor 1 1;
          print_ANSI_string
            "%d0.05%TUTORIAL%n%n%d0.5%%d0.01%_____________________________________________________________%n%d0.5%%d0.02%|You \
             are a small ship trying to survive within the vast \
             sea!|%n%d0.5%%d0.02%|Try to survive as long as possible \
             by avoiding the icebergs|%n%d0.5%%d0.02%|%d0%            \
             %d0.02%%cDO NOT FOLLOW THE FATE OF THE \
             TITANIC%w%d0%          \
             %d0.02%|%n%d0.5%%d0.02%|%d0%                        \
             %d0.05%GOOD LUCK!%d0%                         \
             %d0.02%|%n%d0.5%%d0.01%_____________________________________________________________%n%n%n%d0.5%%d0.02%COMMANDS%n%n%d0.5%%d0.02%Operate \
             your ship using your %yWASD %wkeys%n%d0.5%%d0.02%%yW%w - \
             Moves ship up %n%yA%w - Rotates ship \
             counter-clockwise%n%yS%w - Moves ship down%n%yD%w - \
             Rotates ship clockwise%n%yQ%w - Quits the game%n";
          menu ()
      | "2" -> (
          ANSITerminal.erase Screen;
          ANSITerminal.set_cursor 1 1;
          print_ANSI_string
            "%d0.05%SETTINGS%n%d0.5%%d0.01%--------\n\
             What mode do you want to play in? \n\
             [1] - Easy\n\
             [2] - Intermediate\n\
             [3] - Advanced \n\
             [4] - Back \n\n";
          match read_line () with
          | "1" -> init_game Easy 800 450
          | "2" -> init_game Regular 800 450
          | "3" -> init_game Advanced 800 450
          | "4" -> menu ()
          | _ -> ())
      | _ -> exit 0)

let rec printHelp char =
  match char with
  | 'A' -> ANSITerminal.print_string [ ANSITerminal.blue ] "██"
  | 'B' -> ANSITerminal.print_string [ ANSITerminal.blue ] "████"
  | 'C' -> ANSITerminal.print_string [ ANSITerminal.blue ] "██████"
  | '1' -> ANSITerminal.print_string [ ANSITerminal.yellow ] "██"
  | '2' -> ANSITerminal.print_string [ ANSITerminal.yellow ] "████"
  | '3' -> ANSITerminal.print_string [ ANSITerminal.yellow ] "██████"
  | '0' -> print_endline ""
  | _ -> print_endline ""

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\n\
     ✧･ﾟ: *✧･ﾟ:*✧･ﾟ: *✧･ﾟ:*✧･ﾟ: *✧･ﾟ:*✧･ﾟ: *✧･ﾟ:*\n\
     ✧･ﾟ: *✧･ﾟ:* WELCOME TO THE ODYSSEA *:･ﾟ✧*:･ﾟ✧\n\
     ✧･ﾟ: *✧･ﾟ:*✧･ﾟ: *✧･ﾟ:*✧･ﾟ: *✧･ﾟ:*✧･ﾟ: *✧･ﾟ:*\n\
    \ \n";
  ANSITerminal.erase Screen;
  ANSITerminal.set_cursor 1 1;
  String.iter printHelp
    ("BBBBBBBBBBBBBBBBBB" ^ "0BBBBBBBBBBBBBBBBBB"
   ^ "0AAA3A2B1C1A3A3A3B2BAA" ^ "0AAA1A1A1A1B1A1B1C1C1C1B1AAA"
   ^ "0AAA1A1A1B1B1C3A3A3A31AAA" ^ "0AAA1A1A1A1C1CB1C1A1C1B1AAA"
   ^ "0AAA3A2CA1C3A3A3A1B1AAA" ^ "0BBBBBBBBBBBBBBBBBB"
   ^ "0BBBBBBBBBBBBBBBBBB");
  try menu ()
  with _ ->
    print_endline
      "Thank you for playing oddysea. We hope to see you again!"

(** [window s d1 d2 d3] is ()*)
let window s d1 d2 d3 =
  let open Raylib.Rectangle in
  let open Raylib.Texture in
  let start_button = load_texture s in
  let start_button_rec =
    create 0. 0.
      (float_of_int (width start_button))
      (float_of_int (height start_button))
  in
  let start_button_bounds =
    create
      ((800. /. d1) -. (float_of_int (width start_button) /. d1))
      ((450. /. d3) -. (float_of_int (height start_button) /. d1))
      (float_of_int (width start_button))
      (float_of_int (height start_button))
  in
  set_target_fps 60;
  (start_button, start_button_bounds, start_button_rec, d1, d2, d3)

let rec main_menu () =
  let a, b, c, d, e, f = window "img/startbutton.png" 2. 3. 2. in
  let a1, b1, c1, d1, e1, f1 = window "img/logo.png" 2. 3. 5. in
  let a2, b2, c2, d2, e2, f2 = window "img/tutorial.png" 2. 3. 1.3 in
  let a3, b3, c3, d3, e3, f3 =
    window "img/startbutton.png" 2. 3. 1.23
  in
  let a4, b4, c4, d4, e4, f4 = window "img/back.png" 1.02 3. 1.10 in
  loop [| a; a1; a2; a3; a4 |] [| b; b1; b2; b3; b4 |]
    [| c; c1; c2; c3; c4 |] [| d; d1; d2; d3; d4 |]
    [| e; e1; e2; e3; e4 |] [| f; f1; f2; f3; f4 |]

and loop button button_bounds button_rec d1 d2 d3 =
  let open Raylib.Rectangle in
  let open Raylib.Texture in
  (* draw_text "ODYSSEA" 200 70 85 Color.blue; *)
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let currentmouse = get_mouse_position in
      if
        check_collision_point_rec (currentmouse ()) button_bounds.(0)
        && is_mouse_button_released Left
      then level_load ()
      else if
        check_collision_point_rec (currentmouse ()) button_bounds.(2)
        && is_mouse_button_released Left
      then
        tutorial
          [| button.(3); button.(4) |]
          [| button_bounds.(3); button_bounds.(4) |]
          [| button_rec.(3); button_rec.(4) |]
          [| d1.(3); d1.(4) |]
          [| d2.(3); d2.(4) |]
          [| d3.(3); d3.(4) |]
          false;
      begin_drawing ();
      clear_background Color.white;
      draw_texture_rec button.(0) button_rec.(0)
        (Vector2.create
           ((800. /. d1.(0))
           -. (float_of_int (width button.(0)) /. d1.(0)))
           ((450. /. d3.(0))
           -. (float_of_int (height button.(0)) /. d1.(0))))
        Color.white;
      draw_texture_rec button.(1) button_rec.(1)
        (Vector2.create
           ((800. /. d1.(1))
           -. (float_of_int (width button.(1)) /. d1.(1)))
           ((450. /. d3.(1))
           -. (float_of_int (height button.(1)) /. d1.(1))))
        Color.white;
      draw_texture_rec button.(2) button_rec.(2)
        (Vector2.create
           ((800. /. d1.(2))
           -. (float_of_int (width button.(2)) /. d1.(2)))
           ((450. /. d3.(2))
           -. (float_of_int (height button.(2)) /. d1.(2))))
        Color.white;
      clear_background Color.white;
      end_drawing ();
      loop button button_bounds button_rec d1 d2 d3

and level_menu button button_bounds button_rec d1 d2 d3 =
  let open Raylib.Rectangle in
  let open Raylib.Texture in
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let currentmouse = get_mouse_position in
      if
        check_collision_point_rec (currentmouse ()) button_bounds.(0)
        && is_mouse_button_down Left
      then init_game Easy 800 450
      else if
        check_collision_point_rec (currentmouse ()) button_bounds.(1)
        && is_mouse_button_down Left
      then init_game Regular 800 450
      else if
        check_collision_point_rec (currentmouse ()) button_bounds.(2)
        && is_mouse_button_down Left
      then init_game Advanced 800 450
      else if
        check_collision_point_rec (currentmouse ()) button_bounds.(3)
        && is_mouse_button_down Left
      then main_menu ();

      begin_drawing ();
      clear_background Color.blue;
      draw_texture_rec button.(0) button_rec.(0)
        (Vector2.create
           ((800. /. d1.(0))
           -. (float_of_int (width button.(0)) /. d1.(0)))
           ((450. /. d3.(0))
           -. (float_of_int (height button.(0)) /. d1.(0))))
        Color.white;
      draw_texture_rec button.(1) button_rec.(1)
        (Vector2.create
           ((800. /. d1.(1))
           -. (float_of_int (width button.(1)) /. d1.(1)))
           ((450. /. d3.(1))
           -. (float_of_int (height button.(1)) /. d1.(1))))
        Color.white;
      draw_texture_rec button.(2) button_rec.(2)
        (Vector2.create
           ((800. /. d1.(2))
           -. (float_of_int (width button.(2)) /. d1.(2)))
           ((450. /. d3.(2))
           -. (float_of_int (height button.(2)) /. d1.(2))))
        Color.white;
      draw_texture_rec button.(3) button_rec.(3)
        (Vector2.create
           ((800. /. d1.(3))
           -. (float_of_int (width button.(3)) /. d1.(3)))
           ((450. /. d3.(3))
           -. (float_of_int (height button.(3)) /. d1.(3))))
        Color.white;
      clear_background Color.raywhite;
      end_drawing ();
      level_menu button button_bounds button_rec d1 d2 d3

and level_load () =
  let a, b, c, d, e, f = window "img/easybutton.png" 2. 4. 5. in
  let a1, b1, c1, d1, e1, f1 = window "img/medbutton.png" 2. 3. 2. in
  let a2, b2, c2, d2, e2, f2 = window "img/hardbutton.png" 2. 3. 1.25 in
  let a3, b3, c3, d3, e3, f3 = window "img/back.png" 1.02 3. 1.12 in
  level_menu [| a; a1; a2; a3 |] [| b; b1; b2; b3 |] [| c; c1; c2; c3 |]
    [| d; d1; d2; d3 |] [| e; e1; e2; e3 |] [| f; f1; f2; f3 |]

and tutorial button button_bounds button_rec d1 d2 d3 clicked =
  let open Raylib.Rectangle in
  let open Raylib.Texture in
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let currentmouse = get_mouse_position in
      if clicked && is_mouse_button_up Left then level_load ()
      else if
        check_collision_point_rec (currentmouse ()) button_bounds.(1)
        && is_mouse_button_down Left
      then main_menu ();
      begin_drawing ();
      clear_background Color.white;

      draw_texture_rec button.(0) button_rec.(0)
        (Vector2.create
           ((800. /. d1.(0))
           -. (float_of_int (width button.(0)) /. d1.(0)))
           ((450. /. d3.(0))
           -. (float_of_int (height button.(0)) /. d1.(0))))
        Color.white;
      draw_texture_rec button.(1) button_rec.(1)
        (Vector2.create
           ((800. /. d1.(1))
           -. (float_of_int (width button.(1)) /. d1.(1)))
           ((450. /. d3.(1))
           -. (float_of_int (height button.(1)) /. d1.(1))))
        Color.white;
      let message = "TUTORIAL" in
      let message1 =
        " \
         __________________________________________________________________\n\
        \   You are a small ship trying to survive within the  vast \
         sea! Try to survive \n\
        \   as long as possible by avoiding sharks and icebergs. You \
         may also run into \n\
        \   different obstacles along the way. Pick up items dropped \
         by the gods to  \n\
        \   ease your journey. Beware of the shark watch your health \
         in the \n\
        \   top left corner of the screen \n\
        \                 ESCAPE THE FATE OF THE TITANIC! GOODLUCK! \n\
         _________________________________________________________________"
      in
      let message2 =
        "Navigate the ship using the WASD keys\n\
        \ W - Foward \n\
        \ A - Rotate Counterclockwise \n\
        \ S - Backwards \n\
        \ D - Rotate Clockwise "
      in
      let message3 =
        "Items dropped by the big guys\n\
        \ Zeus' Bolt - 2x speed for 5s \n\
        \ Poseidon's Trident - freeze icebergs\n\
        \ Hera's diadem - 5s of invincibility\n\
        \ Hestia's Fire - melts nearby icebergs"
      in
      draw_text message 327 12 30 Color.blue;
      draw_text message1 120 35 15 Color.maroon;
      draw_text message2 110 209 15 Color.blue;
      draw_text message3 420 209 15 Color.gold;

      clear_background Color.white;
      end_drawing ();
      tutorial button button_bounds button_rec d1 d2 d3
        (clicked
        || check_collision_point_rec (currentmouse ()) button_bounds.(0)
           && is_mouse_button_down Left)

(* Execute the game engine. *)
let () =
  Random.self_init ();
  let open Raylib.Rectangle in
  let open Raylib.Texture in
  init_window 800 450 "ODYSSEA";
  main_menu ()
