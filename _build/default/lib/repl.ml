
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
      | x -> help @@ ((unwrap @@ read_char ()) :: x)
    in
    help []

  let print_esc s =
    printf "%c%s%!" '\027' s

  let rec loop (acc: char list) (hist: t) =
    printf "\r";
    L.prompt ();
    print_esc "[K";
    printf "%s%!" @@ String.of_char_list (List.rev (acc));
    match read_char () with
    | None -> L.stop ()
    | Some c -> (match (c :: acc) with
        | '\n' :: _tl ->
          printf "\n%!";
          let s = String.of_char_list (List.rev acc) in
          s |> L.parse |> L.loop;
          loop [] (History.add s hist)
        | '\127' :: _ :: tl -> loop tl hist
        | '\027' :: tl -> (match read_code () with
            | Cursor_up -> (match History.page_backward hist with
                | (None, hist') -> loop [] hist'
                | (Some s, hist') -> loop (List.rev @@ String.to_list s) hist')
            | Cursor_down -> (match History.page_forward hist with
                | (None, hist') -> loop [] hist'
                | (Some s, hist') -> loop (List.rev @@ String.to_list s) hist')
            | Cursor_forward -> printf "hi\n"; print_esc "[C"; loop tl hist
            | Cursor_backward -> print_esc "[D"; loop tl hist
          )
        | x :: _tl -> loop (x :: acc) hist
        | [] -> L.stop ()
      )

  let start () : unit = loop [] History.empty
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
