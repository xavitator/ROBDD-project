(* File lexer.mll *)

{
  open Parser;;
  exception Eof;;
}

rule token = parse
    [' ' '\t' '\n']      { token lexbuf }
  | 'c'                  { comm lexbuf }
  | '%'                  { PC }
  | 'p'                  { P }
  | ['a'-'z']+           { STRING }
  | '0'                  { ZERO }
  | ['0'-'9']+ as lxm    { INT(int_of_string lxm) }
  | '-'                  { MINUS }
  | eof                  { raise Eof }

and comm = parse
    '\n'                 { token lexbuf }
  | _                    { comm lexbuf }

