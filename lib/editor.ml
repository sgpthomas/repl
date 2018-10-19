open Core

type t = {
  buffer: char list;
  cursor: int
}

let empty = { buffer = []; cursor = 0 }
let create s =
  let charl = String.to_list s in
  { buffer = charl; cursor = List.length charl }

let show t =
  Printf.printf "%s%!" (String.of_char_list t.buffer);
  let lefts = List.length t.buffer - t.cursor in
  let rec apply n =
    if n > 0 then
      (Escape.move_backward (); apply (n-1))
    else
      ()
  in
  apply lefts

let to_string t = String.of_char_list t.buffer

let cursor_start t = { t with cursor = 0 }
let cursor_end t = { t with cursor = (List.length t.buffer) }
let cursor_forward t =
  { t with cursor =
             if t.cursor + 1 > List.length t.buffer then t.cursor
             else t.cursor + 1 }
let cursor_backward t =
  { t with cursor =
             if t.cursor - 1 < 0 then 0
             else t.cursor - 1 }

let insert t c =
    { t with buffer =
               let (l, r) = List.split_n t.buffer t.cursor in
               l @ [c] @ r }
    |> cursor_forward

let remove_last l =
  match l |> List.rev |> List.tl with
  | None -> []
  | Some l -> List.rev l

let delete t =
  (if t.cursor = 0 then t
   else if t.cursor >= (List.length t.buffer) then
     { t with buffer = remove_last t.buffer }
   else
     { t with buffer =
                let (l, r) = List.split_n t.buffer t.cursor in
                (remove_last l) @ r })
  |> cursor_backward

