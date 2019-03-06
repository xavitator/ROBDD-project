(*Implementation des Algorithmes demandés*)

(*Remarque : 
  - n, t et h sont trois references
  - Modifier les structures de t, h et g (Dans apply) --> Voir array Sauf pour h (Pas possible)
*)


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


type exp = N of int | Var of int | No of exp | Et of (exp * exp) | Ou of (exp * exp) | Im of (exp * exp) | Eq of (exp * exp)
(*Voir pour mettre aussi le nb de Variable*)


(*Fonctions de Base*)

type node = int;;


let unite (u : node) : bool =
  u = 0 || u = 1


(** Mettre en HastTbl *)



let t = Hashtbl.create 4096

let h = Hashtbl.create 4096

let n : int ref = ref 0

(** changement *)
let initT : unit = 
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


let initH : unit  =
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

(*let build (f : ?) (n : int) : node = (*n = Nb Variable*)
  let rec aux (f : ?) (i : int) : node =
    if i > !n then if f is false ??? then 0 else 1
    else 
    let v0 = aux (f[0/xi]???, i+1) in
    let v1 = aux (f[1/xi]???, i+1) in
    mk i v0 v1
  in aux f 1*)

let apply (op : node->node->node) (u1 : node) (u2 : node) : node =
  let g = Hashtbl.create 4096 in
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




