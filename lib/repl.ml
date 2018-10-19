
open Core

let rec repl ~f =
  f (); repl ~f

module type Looper = sig
  type t
  val in_ch : In_channel.t
  val out_ch : Out_channel.t
  val parse : string -> t
  val prompt : unit -> unit
  val stop : unit -> unit
  val loop : t -> unit
end

module type Loop = sig
  type t
  val start : unit -> unit
end

module Make (L : Looper) : Loop = struct
  type t = string History.t

  type ansi_code = Cursor_up
                 | Cursor_down
                 | Cursor_forward
                 | Cursor_backward
                 | Cursor_pos of int * int

  let read_char () =
    let termio = UnixLabels.tcgetattr Unix.stdin in
    let () =
      UnixLabels.tcsetattr Unix.stdin ~mode:UnixLabels.TCSAFLUSH
        { termio with UnixLabels.c_icanon = false;
                      UnixLabels.c_echo = false; }
    in
    let res = In_channel.input_char L.in_ch in
    UnixLabels.tcsetattr Unix.stdin ~mode:UnixLabels.TCSAFLUSH termio;
    res

  let read_code () =
    let unwrap = function
      | Some x -> x
      | None -> failwith "Reading error code failed!"
    in
    let rec help = function
      | [] -> help [unwrap @@ read_char ()]
      | ['A';'['] -> Cursor_up
      | ['B';'['] -> Cursor_down
      | ['C';'['] -> Cursor_forward
      | ['D';'['] -> Cursor_backward
      | ['A'; col;';'; ln;'['] -> Cursor_pos (Char.get_digit_exn ln, Char.get_digit_exn col)
      | ['B'; col;';'; ln;'['] -> Cursor_pos (Char.get_digit_exn ln, Char.get_digit_exn col)
      | ['C'; col;';'; ln;'['] -> Cursor_pos (Char.get_digit_exn ln, Char.get_digit_exn col)
      | ['D'; col;';'; ln;'['] -> Cursor_pos (Char.get_digit_exn ln, Char.get_digit_exn col)
      | x -> help @@ ((unwrap @@ read_char ()) :: x)
    in
    help []

  let rec loop (acc: Editor.t) (hist: t) =
    printf "\r";
    L.prompt ();
    Escape.clear_line ();
    Editor.show acc;
    match read_char () with
    | None -> L.stop ()
    | Some c -> (match c with
        | '\n' ->
          printf "\n%!";
          let s = Editor.to_string acc in
           s |> L.parse |> L.loop;
          loop (Editor.empty) (History.add s hist)
        | '\027' -> (match read_code () with
            | Cursor_pos (_ln, _col) -> loop acc hist
            | Cursor_up -> (match History.page_prev hist with
                | (None, hist') -> loop Editor.empty hist'
                | (Some s, hist') -> loop (Editor.create s) hist')
            | Cursor_down -> (match History.page_next hist with
                | (None, hist') -> loop Editor.empty hist'
                | (Some s, hist') -> loop (Editor.create s) hist')
            | Cursor_forward -> loop (Editor.cursor_forward acc) hist
            | Cursor_backward -> loop (Editor.cursor_backward acc) hist
          )
        | '\001' -> loop (Editor.cursor_start acc) hist
        | '\004' -> printf "EOF\n"; L.stop ()
        | '\005' -> loop (Editor.cursor_end acc) hist
        | '\127' -> loop (Editor.delete acc) hist
        | x -> loop (Editor.insert acc x) hist
      )
    (* | Some c -> (match (c :: acc) with
     *     | '\n' :: _tl ->
     *       printf "\n%!";
     *       let s = String.of_char_list (List.rev acc) in
     *       s |> L.parse |> L.loop;
     *       loop [] (History.add s hist)
     *     | '\127' :: _ :: tl -> loop tl hist
     *     | '\027' :: tl -> (match read_code () with
     *         | Cursor_up -> (match History.page_prev hist with
     *             | (None, hist') -> loop [] hist'
     *             | (Some s, hist') -> loop (List.rev @@ String.to_list s) hist')
     *         | Cursor_down -> (match History.page_next hist with
     *             | (None, hist') -> loop [] hist'
     *             | (Some s, hist') -> loop (List.rev @@ String.to_list s) hist')
     *         | Cursor_forward ->
     *           Escape.move_forward ();
     *           loop tl hist
     *         | Cursor_backward ->
     *           Escape.move_backward ();
     *           loop tl hist
     *       )
     *     | x :: _tl -> loop (x :: acc) hist
     *     | [] -> L.stop ()
     *   ) *)

  (* let rec loop2 (acc: Editor.t) (hist: t) =
   *   printf "\r";
   *   L.prompt ();
   *   print_esc "[K" *)

  let start () : unit = loop Editor.empty History.empty
end

module StdLooper : (Looper with type t = string) = struct
  type t = string
  let in_ch = In_channel.stdin
  let out_ch = Out_channel.stdout
  let parse s = s
  let prompt () = Printf.printf "> "
  let stop () = Printf.printf "quit"
  let loop _ = ()
end
