type exp =
  | T of bool
  | Var of int
  | No of exp
  | Et of (exp * exp)
  | Ou of (exp * exp)
  | Im of (exp * exp)
  | Eq of (exp * exp)

val main :
  exp -> int -> unit
