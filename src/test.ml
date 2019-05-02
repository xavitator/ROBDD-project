open Implementation

(*Test*)

module Test_exp : InterfaceROBDD with type t = int = struct
  (*type var = int*)
  type t = int
  let compare x y = y - x (*Gerer l'ordre ici*)
  let t_to_string = string_of_int
end

module T = ROBDD (Test_exp)

let tot : int exp = Eq(Et(Im(Var 1, No(Var 0)), Ou(Var 0, No(Var 1))), No(Var 1));;
(*((B->-A)^(Av-B))<->-B totologie*)

let ant : int exp = Eq(Et(Im(Var 1, No(Var 0)), Ou(Var 0, No(Var 1))), Var 1);;
(*((B->-A)^(Av-B))<->B antilogie*)

let impl : int exp = Im(Im(Im(Var 0, Var 1), Im(Var 1, Var 2)), Im(Var 0, Var 2));;
(*((A->B)->(B->C))->(A->C) Faux pour A Vrai et B et C Faux*)

let big : int exp = Eq(Et(Im(Var 0, Var 1), No(Var 4)), Im(Ou(No(Var 1), Var 3), Im(Et(Var 0, No(Var 2)), Eq(Var 2, Ou(Var 4, No(Var 3))))));;
(*(((A->B)^-E)<->((-BvD)->((A^-C)->(c<->(Ev-D)))))*)


let main (e : Test_exp.t exp) (str : string): unit = T.re_init (); T.main_expression e (str ^ ".dot")

let _ = main tot "tot"
let _ = main ant "ant"
let _ = main impl "impl"
let _ = main big "big"