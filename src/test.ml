open Implementation

(*Test*)

module Test_exp : InterfaceROBDD with type t = int = struct
  (*type var = int*)
  type t = int
  let compare x y = x - y (*Gerer l'ordre ici*)
  let t_to_string x = "x" ^ string_of_int x
end

module T = ROBDD (Test_exp)

let tot : int exp = Eq(Et(Im(Var 1, No(Var 0)), Ou(Var 0, No(Var 1))), No(Var 1));;
(*((B->-A)^(Av-B))<->-B totologie*)

let ant : int exp = Eq(Et(Im(Var 1, No(Var 0)), Ou(Var 0, No(Var 1))), Var 1);;
(*((B->-A)^(Av-B))<->B antilogie*)

let impl : int exp = Im(Im(Im(Var 0, Var 1), Im(Var 1, Var 2)), Im(Var 0, Var 2));;
(*((A->B)->(B->C))->(A->C) Faux pour A Vrai et B et C Faux*)

let impl_simple : int exp = Im(Var 0, Var 1)

let big : int exp = Eq(Et(Im(Var 0, Var 1), No(Var 4)), Im(Ou(No(Var 1), Var 3), Im(Et(Var 0, No(Var 2)), Eq(Var 2, Ou(Var 4, No(Var 3))))));;
(*(((A->B)^-E)<->((-BvD)->((A^-C)->(c<->(Ev-D)))))*)

let main (e : Test_exp.t exp) (str : string): unit = T.re_init (); T.main_expression e ("rapport/exemple/" ^ str ^ ".dot")

let worse n = main (Worse.worse n) ("worse_"^string_of_int n)

let xor n = main (Xor.create n) ("xor_"^string_of_int n)

let combinatoire k n = main (Combinat.create k n) ("combinatoire_" ^ string_of_int k ^ "_" ^ string_of_int n)

let queen n = main (Queen.main_queen n) ("queen_"^ string_of_int n)

(* let _ = main impl_simple "impl_simple" *)
(* let _ = main tot "tot"
   let _ = main ant "ant"
   let _ = main impl "impl"
   let _ = main big "big" *)
(* let _ = worse 15 *)

let ordre () =
  let permut = Worse.all_permut [|1;3;4;2|] in
  List.iteri 
    (fun i tab -> 
       print_endline (string_of_int tab.(0) ^ string_of_int tab.(1) ^ string_of_int tab.(2) ^ string_of_int tab.(3));
       let all_op : int exp = Im((Eq((Et(Var tab.(0), Var tab.(1))),(Ou(Var tab.(1), Var tab.(2))))), (Et(No (Var tab.(3)), Var tab.(0)))) in
       (* ((A ^ B) <-> (B v C)) -> (-D ^ A) *)
       main all_op ("all_op_o"^ (string_of_int (i+1)))
    ) permut

let _ = queen 4