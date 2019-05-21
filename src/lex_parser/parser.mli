type token =
  | INT of (int)
  | MINUS
  | STRING
  | ZERO
  | P
  | PC

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (int* int Implementation.exp)
