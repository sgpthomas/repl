

(** Type for escape codes. *)
type code = ClearLine
          | MoveCursorUp
          | MoveCursorDown
          | MoveCursorForward
          | MoveCursorBackward
          | MoveCursorTo of int * int

(** Prints out an escape code. *)
val print : code -> unit

val clear_line : unit -> unit
val move_up : unit -> unit
val move_down : unit -> unit
val move_forward : unit -> unit
val move_backward : unit -> unit
