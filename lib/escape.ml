
type code = ClearLine
          | MoveCursorUp
          | MoveCursorDown
          | MoveCursorForward
          | MoveCursorBackward
          | MoveCursorTo of int * int

let print_esc s =
  Printf.printf "%c%s%!" '\027' s

let str_of_code = function
  | ClearLine -> "[K"
  | MoveCursorUp -> "[A"
  | MoveCursorDown -> "[B"
  | MoveCursorForward-> "[C"
  | MoveCursorBackward -> "[D"
  | MoveCursorTo (ln, col) -> Printf.sprintf "[D%d;%d" ln col

let print c =
  print_esc (str_of_code c)

let clear_line () = print ClearLine
let move_up () = print MoveCursorUp
let move_down () = print MoveCursorDown
let move_forward () = print MoveCursorForward
let move_backward () = print MoveCursorBackward
