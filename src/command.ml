open Raylib

type t =
  | Up
  | Down
  | Left
  | Right

(*association list for [check_key ()]*)
let key_dir_assoc : (Key.t * t) list =
  [ (W, Up); (S, Down); (A, Left); (D, Right) ]

let check_key () : t list =
  let rec aux_check assoc keys =
    match assoc with
    | [] -> keys
    | (k, d) :: t ->
        if is_key_down k then aux_check t (d :: keys)
        else aux_check t keys
  in
  aux_check key_dir_assoc []
