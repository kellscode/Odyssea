exception UnknownEscapeChar of string

let is_color = function
  | ANSITerminal.Foreground s -> true
  | _ -> false

let is_on_color = function
  | ANSITerminal.Background s -> true
  | _ -> false

let char_style chr style =
  let style_assoc =
    [
      ("r", ANSITerminal.red);
      ("b", ANSITerminal.blue);
      ("k", ANSITerminal.black);
      ("g", ANSITerminal.green);
      ("y", ANSITerminal.yellow);
      ("m", ANSITerminal.magenta);
      ("c", ANSITerminal.cyan);
      ("w", ANSITerminal.white);
      ("or", ANSITerminal.on_red);
      ("ob", ANSITerminal.on_blue);
      ("ok", ANSITerminal.on_black);
      ("og", ANSITerminal.on_green);
      ("oy", ANSITerminal.on_yellow);
      ("om", ANSITerminal.on_magenta);
      ("oc", ANSITerminal.on_cyan);
      ("ow", ANSITerminal.on_white);
      ("i", ANSITerminal.Inverse);
      ("u", ANSITerminal.Underlined);
      ("l", ANSITerminal.Bold);
    ]
  in
  let new_s = List.assoc chr style_assoc in
  if is_color new_s then
    new_s :: List.filter (fun x -> not (is_color x)) style
  else if is_on_color new_s then
    new_s :: List.filter (fun x -> not (is_on_color x)) style
  else if List.length (List.filter (fun x -> x = new_s) style) > 0 then
    List.filter (fun x -> not (x = new_s)) style
  else new_s :: style

let escape_pause str =
  let rec ep_acc str n =
    if String.sub str n 1 = "%" then (String.sub str 0 n, n)
    else ep_acc str (n + 1)
  in
  let f, n = ep_acc str 0 in
  (Float.of_string f, n + 1)

let print_ANSI_string str =
  let rec print_char str i style delay =
    if i = String.length str then ()
    else
      let chr = String.sub str i 1 in
      if chr = "%" then
        (*parse escape sequence*)
        let escape_chr = String.sub str (i + 1) 1 in
        if escape_chr = "n" then (
          (*new line*)
          ANSITerminal.print_string style "\n";
          print_char str (i + 2) style delay)
        else if escape_chr = "t" then (
          (*tab*)
          ANSITerminal.print_string style "\t";
          print_char str (i + 2) style delay)
        else if escape_chr = "%" then (
          (*percent symbol*)
          ANSITerminal.print_string style "%";
          print_char str (i + 2) style delay)
        else if escape_chr = "d" then (
          (*change delay*)
          let d, n =
            escape_pause
              (String.sub str (i + 2) (String.length str - i - 2))
          in
          Unix.sleepf d;
          print_char str (i + n + 2) style d)
        else if escape_chr = "o" then
          (*change background*)
          let escape_chr = String.sub str (i + 1) 2 in
          try
            let s = char_style escape_chr style in
            print_char str (i + 3) s delay
          with Not_found -> raise (UnknownEscapeChar escape_chr)
        else
          (*change text color*)
          try
            let s = char_style escape_chr style in
            print_char str (i + 2) s delay
          with Not_found -> raise (UnknownEscapeChar escape_chr)
      else begin
        (*print current char*)
        if chr = "\\'" then ANSITerminal.print_string style "\'"
        else ANSITerminal.print_string style chr;
        Printf.printf "%!";
        Unix.sleepf delay;
        print_char str (i + 1) style delay
      end
  in
  print_char str 0 [ ANSITerminal.white ] 0.
