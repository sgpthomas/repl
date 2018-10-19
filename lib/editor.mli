
(** Type of editor *)
type t

(** An empty Editor *)
val empty : t

(** Create an editor from a string. *)
val create : string -> t

(** Prints out editor to console. *)
val show : t -> unit

(** Editor to string. *)
val to_string : t -> string

(***** Cursor Move functions ******)

(** Move cursor to start of editor *)
val cursor_start : t -> t

(** Move cursor to end of editor *)
val cursor_end : t -> t

(** Move cursor one to right in editor *)
val cursor_forward : t -> t

(** Move cursor one to left in editor *)
val cursor_backward : t -> t

(** Inserts char into buffer *)
val insert : t -> char -> t

(** Deletes 1 character at cursor in editor *)
val delete : t -> t
