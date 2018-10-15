
open Core

type 'a t = (int * 'a list)

type result = Reset
            | Value of int

let cap_inc x n =
  if (x + 1) > n then x
  else x + 1

let cap_dec x =
  if (x - 1) < (-1) then Reset
  else Value (x - 1)

let empty = (0, [])
let is_empty = function
  | (0, []) -> true
  | _ -> false

let add e (_pt, lst) = (0, e :: lst)

let page_forward (pt, lst) =
  match lst with
  | [] -> (None, empty)
  | _x -> match cap_dec pt with
    | Reset -> (None, (0, lst))
    | Value x -> (List.nth lst x, (x, lst))

let page_backward (pt, lst) =
  match lst with
  | [] -> (None, empty)
  | _x -> let pt' = cap_inc pt (List.length lst) in
    (List.nth lst pt, (pt', lst))
