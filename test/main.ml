(** Testing Plan:

    This OUnit test suite Runs some basic tests on initial properties,
    properties dependent on screen dimensions, and properties that are
    easily tested with a defined expected outcome. This was done with a
    mix of black-box, glass-box and randomized testing. The properties
    tested are as follows:

    - Module Iceberg:
    - icebergs spawn on the edge - black box
    - icebergs spawn with velocities that bring them across the screen -
      black box
    - icebergs do not overlap for more than 5 consecutive frames (i.e.
      collisions are handled in 5 frames or less, this is likely the
      most important property tested) - randomized
    - ship hp updating - black box

    - Module Ship:
    - ship starts in middle of screen - black box
    - relative new location of ship for various key inputs and starting
      angles - glass box

    - Module State:
    - relative new location of ships in a state after hitting the edge,
      for both types of edge wrapping- glass box
    - ship hp after touching an iceberg for a consecutive number of
      ticks - glass box

    Given the significant influence of randomess and user input in the
    output of our system, it is hard for us to test the more complex
    elements of what we have. However this testing suite lets us be
    confident in the core elements of our system, which in turn combined
    with the extensive manual testing we have done gives us confidence
    in the correctness of our system.**)

open OUnit2
open Game

(*tests if icebergs spawn on edge*)
let iceberg_edge_test seed x y =
  let open Iceberg in
  Random.init seed;
  let i = Option.get (init_ice 0. x y "" []) in
  let (px, py), s = (ice_pos i, ice_size i) in
  Printf.sprintf "iceberg edge test with seed %i and %.0f x %.0f screen"
    seed x y
  >:: fun _ ->
  assert_equal true
    (px = -2. *. s
    || px = x +. (s *. 2.)
    || py = -2. *. s
    || py = y +. (s *. 2.))
    ~printer:(fun _ ->
      Printf.sprintf
        "spawned at (%f, %f) with size %f, screen is %.0f x %.0f" px py
        s x y)

(*tests if icebergs move into screen*)
let iceberg_dir_test seed x y =
  let open Iceberg in
  Random.init seed;
  let i = Option.get (init_ice 1. x y "" []) in
  let (px1, py1), s = (ice_pos i, ice_size i) in
  let px2, py2 = tick i 0 |> ice_pos in
  Printf.sprintf
    "iceberg direction test with seed %i and %.0f x %.0f screen" seed x
    y
  >:: fun _ ->
  assert_equal true
    (px2 > -2. *. s
    && px2 < x +. (s *. 2.)
    && py2 > -2. *. s
    && py2 < y +. (s *. 2.))
    ~printer:(fun _ ->
      Printf.sprintf "moved from (%f, %f) to (%f, %f)" px1 py1 px2 py2)

(*helper functions for checking collisions*)
let rec incr_coll i1 i2 acc = function
  | [] -> ((i1, i2), 1) :: acc
  | ((h1, h2), n) :: t ->
      if (h1 == i1 && h2 == i2) || (h2 == i1 && h1 == i2) then
        (((h1, h2), n + 1) :: acc) @ t
      else incr_coll i1 i2 (((h1, h2), n) :: acc) t

let rec check_once acc = function
  | [] -> acc
  | h :: t ->
      check_once
        (List.fold_left
           (fun x y ->
             if Iceberg.contact h y then incr_coll h y [] acc
             else
               List.filter
                 (fun ((i1, i2), _) ->
                   ((not (i1 == h)) && not (i2 == y))
                   || ((not (i1 == h)) && not (i1 == y)))
                 acc)
           acc t)
        t

let rec check_collisions st ticks contact =
  let open State in
  let st = tick ticks [] st in
  let lst = check_once contact (icebergs st) in
  let b, s =
    List.fold_left
      (fun x ((i1, i2), n) ->
        if n = 5 then
          let (x1, y1), (x2, y2), s1, s2 =
            Iceberg.(ice_pos i1, ice_pos i1, ice_size i1, ice_size i2)
          in
          ( true,
            Printf.sprintf
              "iceberg 1 at (%f, %f) with size %f.\n\
              \ iceberg 2 at (%f, %f) with size %f" x1 y1 s1 x2 y2 s2 )
        else (fst x || false, snd x))
      (false, "") lst
  in
  if b then (b, s)
  else if ticks = 0 then (true, "")
  else check_collisions st (ticks - 1) contact

(**tests if any icebergs are overlapping for more than 5 frames - i.e.
   assumes any iceberg collisions are solved in less than 6 frames*)
let iceberg_collision_test seed x y num ticks =
  let open Iceberg in
  let open State in
  Random.init seed;
  let b, s = check_collisions (init_state Easy x y num true) ticks [] in
  Printf.sprintf
    "iceberg collision test with seed %i, %.0f x %.0f screen and %i \
     icebergs after %i ticks"
    seed x y num ticks
  >:: fun _ -> assert_equal true b ~printer:(fun _ -> s)

let iceberg_tests =
  let open Random in
  self_init ();
  let s1, s2 = (int 9999999, int 9999999) in
  [
    iceberg_edge_test (int 9999999) 800. 450.;
    iceberg_edge_test (int 9999999) 2000. 2000.;
    iceberg_edge_test (int 9999999) 20. 20.;
    iceberg_edge_test (int 9999999) 1600. 10.;
    iceberg_edge_test (int 9999999) 10. 1600.;
    iceberg_dir_test (int 9999999) 800. 450.;
    iceberg_dir_test (int 9999999) 2000. 2000.;
    iceberg_dir_test (int 9999999) 20. 20.;
    iceberg_dir_test (int 9999999) 1600. 10.;
    iceberg_dir_test (int 9999999) 10. 1600.;
    iceberg_collision_test s1 800. 450. 10 0;
    iceberg_collision_test s1 800. 450. 30 0;
    iceberg_collision_test s1 800. 450. 30 100;
    iceberg_collision_test s1 800. 450. 30 1000;
    iceberg_collision_test s2 800. 450. 10 0;
    iceberg_collision_test s2 800. 450. 30 0;
    iceberg_collision_test s2 800. 450. 30 100;
    iceberg_collision_test s2 800. 450. 30 1000;
    iceberg_collision_test s1 200. 200. 5 0;
    iceberg_collision_test s1 200. 200. 10 0;
    iceberg_collision_test s1 200. 200. 10 100;
    iceberg_collision_test s1 200. 200. 10 1000;
    iceberg_collision_test s2 200. 200. 5 0;
    iceberg_collision_test s2 200. 200. 10 0;
    iceberg_collision_test s2 200. 200. 10 100;
    iceberg_collision_test s2 200. 200. 10 1000;
  ]

(* tests initial ship position for different screen sizes*)
let ship_pos_test x y =
  let open Ship in
  let sx, sy = init_ship x y |> ship_pos in
  Printf.sprintf "ship pos test with %.0f x %.0f screen" x y
  >:: fun _ ->
  assert_equal
    (x /. 2., y /. 2.)
    (sx, sy)
    ~printer:(fun (x, y) -> Printf.sprintf "(%.0f, %.0f)" x y)

(* tests what quadrant ship is in after key commands*)
let ship_move_test name x y m ship_dir x_quad y_quad =
  let open Ship in
  let open Command in
  let s = set_ship_angle (init_ship x y) ship_dir in
  let rec move_ship s = function
    | [] -> s
    | h :: t -> move_ship (move h s |> tick) t
  in
  let sx, sy = move_ship s m |> ship_pos in
  let x_dir =
    match compare sx (x /. 2.) with
    | x when x < 0 -> `Left
    | x when x > 0 -> `Right
    | _ -> `Mid
  in
  let y_dir =
    match compare sy (y /. 2.) with
    | y when y < 0 -> `Down
    | y when y > 0 -> `Up
    | _ -> `Mid
  in
  name >:: fun _ ->
  assert_equal (x_quad, y_quad) (x_dir, y_dir) ~printer:(fun _ ->
      Printf.sprintf "(%.2f, %.2f)" sx sy)

(*tests ship location after hitting edge without edge wrapping*)
let state_edge_test_no_wrap x y dir =
  let open State in
  let open Float in
  let st = init_state Easy x y 0 false in
  let size = ship st |> Ship.ship_size in
  let st =
    match dir with
    | "top" ->
        set_ship_pos st (x /. 2., size) |> fun x ->
        set_ship_angle x (3. *. pi /. 2.)
    | "bottom" ->
        set_ship_pos st (x /. 2., y -. size) |> fun x ->
        set_ship_angle x (pi /. 2.)
    | "left" ->
        set_ship_pos st (size, y /. 2.) |> fun x -> set_ship_angle x pi
    | "right" ->
        set_ship_pos st (x -. size, y /. 2.) |> fun x ->
        set_ship_angle x 0.
    | _ -> failwith "unexpected string"
  in
  let st_n = tick 0 [ Up ] st |> tick 1 [ Up ] in
  Printf.sprintf
    "ship %s edge test with %.0f x %.0f screen and no screen wrapping"
    dir x y
  >:: fun _ ->
  assert_equal
    (Ship.ship_pos (ship st))
    (Ship.ship_pos (ship st_n))
    ~printer:(fun (x, y) -> Printf.sprintf "(%.4f, %.4f)" x y)

(*tests ship location after hitting edge with edge wrapping*)
let state_edge_test_wrap x y dir =
  let open State in
  let open Float in
  let st = init_state Easy x y 0 true in
  let size = ship st |> Ship.ship_size in
  let st, f =
    match dir with
    | "top" ->
        set_ship_pos st (x /. 2., size) |> fun x ->
        ( set_ship_angle x (3. *. pi /. 2.),
          fun x -> Ship.(ship x |> ship_pos |> snd) < y /. 2. )
    | "bottom" ->
        set_ship_pos st (x /. 2., y -. size) |> fun x ->
        ( set_ship_angle x (pi /. 2.),
          fun x -> Ship.(ship x |> ship_pos |> snd) > y /. 2. )
    | "left" ->
        set_ship_pos st (size, y /. 2.) |> fun z ->
        ( set_ship_angle z pi,
          fun z -> Ship.(ship z |> ship_pos |> fst) < x /. 2. )
    | "right" ->
        set_ship_pos st (x -. size, y /. 2.) |> fun z ->
        ( set_ship_angle z 0.,
          fun z -> Ship.(ship z |> ship_pos |> fst) > x /. 2. )
    | _ -> failwith "unexpected string"
  in
  let st_n = tick 0 [ Up ] st |> tick 1 [ Up ] in
  Printf.sprintf
    "ship %s edge test with %.0f x %.0f screen and screen wrapping" dir
    x y
  >:: fun _ -> assert_equal true (f st_n)

(*tests changing ship hp*)
let ship_hp_test name x y func expected =
  let open Ship in
  let s = init_ship x y |> func in
  name >:: fun _ -> assert_equal expected (ship_hp s)

let ship_tests =
  let open Float in
  [
    ship_pos_test 800. 450.;
    ship_pos_test 2000. 2000.;
    ship_pos_test 10. 1600.;
    ship_move_test "ship move test up once" 800. 450. [ [ Up ] ]
      (pi /. 2.) `Mid `Up;
    ship_move_test "ship move test up multiple" 800. 450.
      [ [ Up ]; [ Up ]; [ Up ]; [ Up ] ]
      (pi /. 2.) `Mid `Up;
    ship_move_test "ship move test up and down same time does not move"
      800. 450.
      [ [ Up; Down ]; [ Up; Down ] ]
      (pi /. 2.) `Mid `Mid;
    ship_move_test "ship move test rotations does not move" 800. 450.
      [
        [ Left ];
        [ Right ];
        [ Left; Right ];
        [ Right ];
        [ Left ];
        [ Right ];
        [ Right ];
      ]
      (pi /. 2.) `Mid `Mid;
    ship_move_test "ship move test down once" 800. 450. [ [ Down ] ]
      (pi /. 2.) `Mid `Down;
    ship_move_test "ship move test down multiple" 800. 450.
      [ [ Down ]; [ Down ]; [ Down ]; [ Down ] ]
      (pi /. 2.) `Mid `Down;
    ship_move_test "ship move test rotate left and move forward" 800.
      450.
      [ [ Left ]; [ Left ]; [ Up ]; [ Up ] ]
      (pi /. 2.) `Right `Up;
    ship_move_test "ship move test rotate left and move backward" 800.
      450.
      [ [ Left ]; [ Left ]; [ Down ]; [ Down ] ]
      (pi /. 2.) `Left `Down;
    ship_move_test "ship move test rotate right and move forward" 800.
      450.
      [ [ Right ]; [ Right ]; [ Up ]; [ Up ] ]
      (pi /. 2.) `Left `Up;
    ship_move_test "ship move test rotate right and move backward" 800.
      450.
      [ [ Right ]; [ Right ]; [ Down ]; [ Down ] ]
      (pi /. 2.) `Right `Down;
    ship_move_test "ship move test face down and move forward" 800. 450.
      [ [ Up ] ]
      (3. *. pi /. 2.)
      `Mid `Down;
    ship_move_test "ship move test face right and move forward" 800.
      450. [ [ Up ] ] 0. `Right `Mid;
    ship_move_test "ship move test face left and move forward" 800. 450.
      [ [ Up ] ] pi `Left `Mid;
    ship_move_test "ship move test face northeast and move forward" 800.
      450. [ [ Up ] ] (pi /. 4.) `Right `Up;
    ship_move_test "ship move test face northwest and move forward" 800.
      450. [ [ Up ] ]
      (3. *. pi /. 4.)
      `Left `Up;
    ship_move_test "ship move test face southwest and move forward" 800.
      450. [ [ Up ] ]
      (5. *. pi /. 4.)
      `Left `Down;
    ship_move_test "ship move test face southeast and move forward" 800.
      450. [ [ Up ] ]
      (7. *. pi /. 4.)
      `Right `Down;
    ship_hp_test "initial hp" 800. 450. (fun x -> x) 100;
    ship_hp_test "set hp" 800. 450. (fun x -> Ship.new_hp x 40) 60;
  ]

(*tests ship shp in state when a ship has been touching an iceberg for
  [ticks] frames*)
let state_hp_test x y ticks expected =
  let open State in
  let open Float in
  let s = ref (init_state Easy x y 1 false) in
  let i = ref 0 in
  let j = ref 0 in
  while !i < ticks do
    let ix, iy, is = List.hd (ices_pos !s) in
    s := set_ship_pos !s (ix, iy) |> tick !j [];
    if collide (ship !s) (List.hd (icebergs !s)) then incr i else ();
    incr j
  done;
  Printf.sprintf
    "state ship hp test after contacting iceberg for %i ticks" ticks
  >:: fun _ ->
  assert_equal expected
    (!s |> ship |> Ship.ship_hp)
    ~printer:string_of_int

let state_tests =
  let open Random in
  self_init ();
  let x, y = (300. +. float 2000., 300. +. float 2000.) in
  [
    state_edge_test_no_wrap 850. 400. "top";
    state_edge_test_no_wrap 850. 400. "bottom";
    state_edge_test_no_wrap 850. 400. "left";
    state_edge_test_no_wrap 850. 400. "right";
    state_edge_test_no_wrap 1600. 1600. "top";
    state_edge_test_no_wrap 1600. 1600. "bottom";
    state_edge_test_no_wrap 1600. 1600. "left";
    state_edge_test_no_wrap 1600. 1600. "right";
    state_edge_test_wrap 850. 400. "top";
    state_edge_test_wrap 850. 400. "bottom";
    state_edge_test_wrap 850. 400. "left";
    state_edge_test_wrap 850. 400. "right";
    state_edge_test_wrap 1600. 1600. "top";
    state_edge_test_wrap 1600. 1600. "bottom";
    state_edge_test_wrap 1600. 1600. "left";
    state_edge_test_wrap 1600. 1600. "right";
    state_hp_test 850. 400. 30 90;
    state_hp_test 850. 400. 31 80;
    state_hp_test 850. 400. 60 80;
    state_hp_test 850. 400. 61 70;
    state_hp_test 850. 400. 271 0;
    state_hp_test 850. 400. 0 100;
    state_hp_test 850. 400. 1 90;
    state_hp_test x y 30 90;
    state_hp_test x y 31 80;
    state_hp_test x y 60 80;
    state_hp_test x y 61 70;
    state_hp_test x y 271 0;
    state_hp_test x y 0 100;
    state_hp_test x y 1 90;
  ]

let tests =
  "test suite"
  >::: List.flatten [ iceberg_tests; ship_tests; state_tests ]

let _ = run_test_tt_main tests
