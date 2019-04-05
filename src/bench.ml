(* File calc.ml *)

open Implementation;;

let main (channel : string) : unit =
  let std = open_in channel in
  try
    let lexbuf = Lexing.from_channel std in
    let (n,e) = Parser.main Lexer.token lexbuf in
    Implementation.main e n (channel ^ "_file.dot")
  with _ -> print_endline "Fin !"
;;

if !Sys.interactive
then ()
else if Array.length Sys.argv <> 2 then 
  print_endline "Usage: calc <file>"
else main Sys.argv.(1)
;;
