
open Core

module Map = Map.Make(Int)

type pointer = Pos of int
             | Current

(** Type of history. Biggest number is most recent. *)
type 'a t = {
  count : int;
  pointer : pointer;
  hist : 'a Map.t
}

let empty : 'a t = {
  count = 0;
  pointer = Pos 0;
  hist = Map.empty
}

let point_of_t h =
  match h.pointer with
  | Current -> (-1)
  | Pos i -> i

let is_empty h = h.count = 0

let add e h =
  let n = h.count + 1 in
  {
  count = n;
  pointer = Current;
  hist = match Map.add h.hist ~key:n ~data:e with
    | `Duplicate -> h.hist
    | `Ok s -> s
}

let next_ptr h =
  {
    h with pointer = match h.pointer with
      | Current -> Current
      | Pos i ->
        if (i + 1) > h.count then Current
        else Pos (i + 1)
  }

let prev_ptr h =
  {
    h with pointer = match h.pointer with
      | Current -> Pos h.count
      | Pos i ->
        if (i - 1) < 0 then Pos 0
        else Pos (i - 1)
  }

let page_next h =
  let h' = next_ptr h in
  (Map.find h'.hist (point_of_t h'), h')

let page_prev h =
  let h' = prev_ptr h in
  (Map.find h'.hist (point_of_t h'), h')
