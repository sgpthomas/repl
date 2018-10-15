
open Core

module L : Repl.Looper = struct
  include Repl.StdLooper
  let loop x = Printf.printf "%s\n" x
end

module Loop = Repl.Make(L)

let () =
  Loop.start ()
