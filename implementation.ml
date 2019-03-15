(*Implementation des Algorithmes demandés*)

let map_size = 4096;;

(*Fonctions de Language Binaire*)

let no (x : int) : int = (*Pas utile ici*)
  match x with 0 -> 1 | _ -> 0

let et (x : int) (y : int) : int =
  match x with 0 -> 0 | _ -> y

let ou (x : int) (y : int) : int =
  match x with 0 -> y | _ -> 1

let im (x : int) (y : int) : int =
  match x with 0 -> 1 | _ -> y

let eq (x : int) (y : int) : int =
  match x with 0 -> no y | _ -> y


(** faire match et interpretation partielle *)


type exp = T of bool | Var of int | No of exp | Et of (exp * exp) | Ou of (exp * exp) | Im of (exp * exp) | Eq of (exp * exp)

let rec getBool (e : exp) (tab : bool option array) : bool option =
  match e with
  | T b -> Some b
  | Var i -> tab.(i)
  | No e -> begin
      match getBool e tab with
      | None -> None
      | Some b -> Some (not b)
    end
  | Et (e1, e2) -> begin
      match getBool e1 tab with
      | None -> None
      | Some b -> if b then getBool e2 tab else Some false
    end
  | Ou (e1, e2) -> begin 
      match getBool e1 tab with 
      | None -> begin match getBool e2 tab with None -> None | Some b -> if b then Some true else None end
      | Some b -> if b then Some true else getBool e2 tab
    end
  | Im (e1, e2) -> begin
      match (getBool e1 tab, getBool e2 tab) with
      | (_, Some b) when b -> Some true
      | (Some b, _) when (not b) -> Some true
      | (None, _) | (_, None) -> None
      | _ -> Some false
    end
  | Eq (e1, e2) -> begin
      match (getBool e1 tab, getBool e2 tab) with
      | (None, _) | (_, None) -> None
      | (Some b1, Some b2) -> Some (b1=b2)
    end


(*Fonctions de Base*)

type node = int;;


let unite (u : node) : bool =
  u = 0 || u = 1


(** Mettre en HastTbl *)



let t = Hashtbl.create map_size

let h = Hashtbl.create map_size

let n : int ref = ref 0

(** changement *)
let initT () : unit = 
  begin
    Hashtbl.add t 0 (!n + 1, 0, 0); 
    Hashtbl.add t 1 (!n + 1, 0, 0)
  end

let add (i : int) (l : node) (k : node) : node = 
  let res = Hashtbl.length t in
  Hashtbl.add t res (i,l,k); res

let var (u : node) : int =
  let (i,l,h) = Hashtbl.find t u in i

let low (u : node) : node =
  let (i,l,h) = Hashtbl.find t u in l

let high (u : node) : node =
  let (i,l,h) = Hashtbl.find t u in h


let initH () : unit  =
  begin
    Hashtbl.add h (!n + 1, 0, 0) 0; 
    Hashtbl.add h (!n + 1, 0, 0) 1
  end

let member (i : int) (l : node) (k : node) : bool =
  Hashtbl.mem h (i,l,k) 

let lookup (i : int) (l : node) (k : node) : node =
  Hashtbl.find h (i,l,k)

let insert (i : int) (l : node) (k : node) (u : node) : unit =
  Hashtbl.replace h (i,l,k) u

let rec two_power = function 0 -> 1 | x -> 2 * two_power (x - 1)


(*Fonctions Demandées*)
let mk (i : int) (l : node) (k : node) : node =
  if l = k then l
  else
  if member i l k then lookup i l k
  else
    let u = add i l k in
    insert i l k u;
    u

let build (f : exp) : node =
  let rec aux (f : exp) (i : int) (tab : bool option array): node =
    match getBool f tab with
    | None -> begin
        if i >= (!n) then failwith "exception de getBool"
        else
          let v0 = tab.(i) <- Some false; aux f (i+1) tab in
          let v1 = tab.(i) <- Some false; aux f (i+1) tab in
          tab.(i) <- None;
          mk i v0 v1
      end
    | Some b -> if b then 1 else 0
  in
  let tab = Array.init (!n) (function a -> None) in
  aux f 0 tab

let apply (op : node->node->node) (u1 : node) (u2 : node) : node =
  let g = Hashtbl.create (map_size*map_size) in
  let rec aux (u1 : node) (u2 : node) : node =
    if Hashtbl.mem g (u1, u2) then Hashtbl.find g (u1, u2)
    else
      let u =
        if unite u1 && unite u2 then op u1 u2
        else if var u1 = var u2 then 
          mk (var u1)
            (aux (low u1) (low u2)) 
            (aux (high u1) (high u2))
        else if var u1 < var u2 then 
          mk (var u1)
            (aux (low u1) u2) 
            (aux (high u1) u2)
        else 
          mk (var u2)
            (aux u1 (low u2)) 
            (aux u1 (high u2))
      in 
      Hashtbl.add g (u1, u2) u;
      u
  in
  aux u1 u2

let restrict (u : node) (j : int) (b : int) : node = 
  let rec aux (u : node) : node =
    if var u > j then u
    else if var u < j then mk (var u) (aux (low u)) (aux (high u))
    else if b = 0 then aux (low u)
    else aux (high u)
  in
  aux u

let satCount (u : node) : int =
  let rec aux (u : node) : int =
    if u = 0 then 0
    else if u = 1 then 1
    else 
      two_power (var (low u) - var u - 1) * aux (low u) +
      two_power (var (high u) - var u - 1) * aux (high u)
  in aux u

let rec anySat (u : node) : (int* int) list =
  assert (u != 0);
  if u = 1 then []
  else if low u = 0 then (var u, 1) :: (anySat (high u))
  else (var u, 0) :: (anySat (low u))

let allSat (u : node) : (int* int) list list =
  let rec aux (u : node) (l : (int* int) list) (r : (int* int) list list) : (int* int) list list =
    if u = 0 then r
    else if u = 1 then l::r
    else aux (high u) ((var u, 1)::l) (aux (low u) ((var u, 0)::l) r)
  in aux u [] []

let simplify (d : node) (u : node) : node =
  let rec aux (d : node) (u : node) : node =
    if d = 0 then 0
    else if u <= 1 then u
    else if d = 1 then mk (var u) (aux d (low u)) (aux d (high u))
    else if var d = var u then
      if low d = 0 then aux (high d) (high u)
      else if high d = 0 then aux (low d) (low u)
      else mk (var u) (aux (low d) (low u)) (aux (high d) (high u))
    else if var d < var u
    then mk (var u) (aux (low d) u) (aux (high d) u)
    else mk (var u) (aux d (low u)) (aux d (high u))
  in aux d u

(*Affichage simple de ROBDD*)

let aff (n : node) : unit =
  let rec aux (n : node) (esp : string) : unit =
    print_string (esp ^ (string_of_int n));
    if not(unite n) then begin print_endline (":" ^ (string_of_int (var n))); aux (low n) (esp ^ " |"); print_endline ""; aux (high n) (esp ^ "  ")end
  in
  aux n "";
  print_endline ""

(*Affiche : node n : var n
                     low n
                     high n*)

let rec string_of_exp (e : exp) (esp : string): string =
  match e with
  | T b -> string_of_bool b
  | Var n -> "x" ^ string_of_int n
  | No e -> "No(" ^ string_of_exp e (esp ^ "   ") ^ "\n" ^ esp ^ ")"
  | Et (e1,e2) -> "\n" ^ esp ^ "Et(" ^ string_of_exp e1 (esp ^ "  ") ^ "," ^ string_of_exp e2 (esp ^ "  ") ^ ")"
  | Ou (e1,e2) -> "\n" ^ esp ^ "Ou(" ^ string_of_exp e1 (esp ^ "  ") ^ "," ^ string_of_exp e2 (esp ^ "  ") ^ ")"
  | Im (e1,e2) -> "\n" ^ esp ^ "Im(" ^ string_of_exp e1 (esp ^ "  ") ^ "," ^ string_of_exp e2 (esp ^ "  ") ^ ")"
  | Eq (e1,e2) -> "\n" ^ esp ^ "Eq(" ^ string_of_exp e1 (esp ^ "  ") ^ "," ^ string_of_exp e2 (esp ^ "  ") ^ ")"

(*Affiche les exp*)

(*Test*)

let tot : exp = Eq(Et(Im(Var 1, No(Var 0)), Ou(Var 0, No(Var 1))), No(Var 1));;
(*((B->-A)^(Av-B))<->-B totologie*)

let ant : exp = Eq(Et(Im(Var 1, No(Var 0)), Ou(Var 0, No(Var 1))), Var 1);;
(*((B->-A)^(Av-B))<->B antilogie*)

let impl : exp = Im(Im(Im(Var 0, Var 1), Im(Var 1, Var 2)), Im(Var 0, Var 2));;
(*((A->B)->(B->C))->(A->C) Faux pour A Vrai et B et C Faux*)

let big : exp = Eq(Et(Im(Var 0, Var 1), No(Var 4)), Im(Ou(No(Var 1), Var 3), Im(Et(Var 0, No(Var 2)), Eq(Var 2, Ou(Var 4, No(Var 3))))));;
(*(((A->B)^-E)<->((-BvD)->((A^-C)->(c<->(Ev-D)))))*)

let main e i = n := i; initT(); initH(); print_endline (string_of_exp e ""); print_endline "Start Build ..."; 
  let x = build e in aff x; print_endline (string_of_int x)

let _ = main big 5

