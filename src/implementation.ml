type 'a exp =
    T of bool
  | Var of 'a
  | No of 'a exp
  | Et of ('a exp * 'a exp)
  | Ou of ('a exp * 'a exp)
  | Im of ('a exp * 'a exp)
  | Eq of ('a exp * 'a exp)


module type InterfaceROBDD = 
sig
  (*type var //plus tard*)
  type t
  val compare : t -> t -> int
  val t_to_string : t -> string
end

module ROBDD (Interface : InterfaceROBDD) =
struct

  (************************ Types utilisés ***********************)

  type node = int;;

  exception No_SAT of string

  (************************ Variables globales ***********************)

  let map_size = 4096;;

  let t = Hashtbl.create map_size

  let h = Hashtbl.create map_size

  let n : int ref = ref 0

  let var_tab = ref [||]

  let node_to_var = ref [(1,0); (0,0)]

  (******************** Fonctions de Language Binaire avec des nodes **************)

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


  (****************** Fonctions de transfert d'une 'a expression en une node expression ******************)

  let get_variables (e : Interface.t exp) : Interface.t array =
    let arr = ref [||] in
    let rec aux ex =
      match ex with
      | T _ -> ()
      | Var a -> if(Array.mem a !arr) = false then arr := Array.append !arr [|a|]
      | No a -> aux a
      | Et (a,b) | Ou (a,b) | Im (a,b) | Eq (a,b) -> aux a; aux b
    in
    aux e; !arr

  let fill_var_tab (e : Interface.t exp) =
    let arr = get_variables e in
    Array.sort Interface.compare arr;
    var_tab := arr

  let get_index_var (a : Interface.t) : int =
    let rec aux i =
      if i < 0 || i >= Array.length !var_tab then failwith "erreur d'implémentation get_index_var"
      else 
      if !var_tab.(i) = a then i
      else aux (i+1)
    in 
    aux 0

  let translate_var (e : Interface.t exp) : int exp =
    fill_var_tab e;
    let rec aux ex =
      match ex with
      | T b -> T b
      | Var a -> Var (get_index_var a)
      | No a -> No (aux a)
      | Et (a,b) -> Et (aux a, aux b)
      | Ou (a,b) -> Ou (aux a, aux b)
      | Im (a,b) -> Im (aux a, aux b)
      | Eq (a,b) -> Eq (aux a, aux b)
    in
    aux e

  (************************ Fonctions de maintien de la hashtbl des variables et fonctions utilitaires *****************************)

  let initT () : unit = 
    begin
      Hashtbl.clear t;
      Hashtbl.add t 0 (0, 0, 0); 
      Hashtbl.add t 1 (1, 0, 0)
    end

  let initH () : unit  =
    begin
      Hashtbl.clear h;
      Hashtbl.add h (0, 0, 0) 0; 
      Hashtbl.add h (1, 1, 1) 1
    end

  let add (i : int) (l : node) (k : node) : node = 
    let res = Hashtbl.length t in
    Hashtbl.add t res (i,l,k); res

  let var (u : node) : int =
    let (i,_,_) = Hashtbl.find t u in i

  let low (u : node) : node =
    let (_,l,_) = Hashtbl.find t u in l

  let high (u : node) : node =
    let (_,_,h) = Hashtbl.find t u in h

  let member (i : int) (l : node) (k : node) : bool =
    Hashtbl.mem h (i,l,k) 

  let lookup (i : int) (l : node) (k : node) : node =
    Hashtbl.find h (i,l,k)

  let insert (i : int) (l : node) (k : node) (u : node) : unit =
    Hashtbl.replace h (i,l,k) u

  let two_power n = 
    let rec aux i tmp =
      if i <= 0 then tmp
      else aux (i-1) (2*tmp)
    in
    aux n 1

  let unite (u : node) : bool = u = 0 || u = 1

  let rec fill_var i li =
    if i < 0 || i >= Array.length !var_tab then li
    else if List.mem (!var_tab.(i), true) li || List.mem (!var_tab.(i), false) li then fill_var (i+1) ((!var_tab.(i), true) :: li)
    else fill_var (i+1) li

  let add_new_node n i =
    let size = Array.length (!var_tab) in
    node_to_var := (n,size - i) :: !node_to_var 

  let re_init () =
    n := 0;
    var_tab := [||];
    node_to_var := [(1,0); (0,0)];
    initH();
    initT()

  (************************ Fonctions d'affichage *****************************)

  module Graph = Dot.MakeDot (struct let is_lower a b = low a = b let is_unite = unite end)    

  let rec string_of_exp (e : 'a exp) (esp : string) (t_to_string : 'a -> string): string =
    match e with
    | T b -> string_of_bool b
    | Var n -> "x" ^ t_to_string n
    | No e -> "No(" ^ string_of_exp e (esp ^ "    ") t_to_string ^ "\n" ^ esp ^ ")"
    | Et (e1,e2) -> "\n" ^ esp ^ "Et(" ^ string_of_exp e1 (esp ^ "  ") t_to_string ^ "," ^ string_of_exp e2 (esp ^ "  ") t_to_string ^ ")"
    | Ou (e1,e2) -> "\n" ^ esp ^ "Ou(" ^ string_of_exp e1 (esp ^ "  ") t_to_string ^ "," ^ string_of_exp e2 (esp ^ "  ") t_to_string ^ ")"
    | Im (e1,e2) -> "\n" ^ esp ^ "Im(" ^ string_of_exp e1 (esp ^ "  ") t_to_string ^ "," ^ string_of_exp e2 (esp ^ "  ") t_to_string ^ ")"
    | Eq (e1,e2) -> "\n" ^ esp ^ "Eq(" ^ string_of_exp e1 (esp ^ "  ") t_to_string ^ "," ^ string_of_exp e2 (esp ^ "  ") t_to_string ^ ")"

  let aff (n : node) : unit =
    let rec aux (n : node) (esp : string) : unit =
      print_string (esp ^ (string_of_int n));
      if not(unite n) then 
        begin 
          print_endline (":" ^ (Interface.t_to_string !var_tab.(var n)));
          aux (low n) (esp ^ " |"); 
          print_endline ""; 
          aux (high n) (esp ^ "  ")
        end
    in
    aux n "";
    print_endline ""
  (*Affiche : node n : var n
                     | low n
                       high n*)



  let rec getBool (e : node exp) (tab : bool option array) : (bool option * node exp) =
    match e with
    | T b -> (Some b, T b)
    | Var i -> begin 
        match tab.(i) with (*TODO Nop*)
        | None -> (None, Var i)
        | Some b -> (Some b, T b)
      end
    | No e -> begin
        match getBool e tab with
        | (None, e) -> (None, No e)
        | (Some b, _) -> (Some (not b), T (not b))
      end
    | Et (e1, e2) -> begin
        match getBool e1 tab with
        | (None, e1) ->  begin 
            match getBool e2 tab with 
            | (None, e2) -> (None, Et (e1, e2)) 
            | (Some b, _) -> if (not b) then (Some false, T false) else (None, e1) 
          end
        | (Some b, _) -> if b then getBool e2 tab else (Some false, T false)
      end
    | Ou (e1, e2) -> begin 
        match getBool e1 tab with 
        | (None, e1) -> begin 
            match getBool e2 tab with 
            | (None, e2) -> (None, Ou (e1, e2)) 
            | (Some b, _) -> if b then (Some true, T true) else (None, e1) 
          end
        | (Some b, _) -> if b then (Some true, T true) else getBool e2 tab
      end
    | Im (e1, e2) -> begin
        match getBool e1 tab with
        | (Some b, _) -> if b then getBool e2 tab else (Some true, T true)
        | (None, e1) -> begin 
            match getBool e2 tab with
            | (Some b, e2) -> if b then (Some true, T true) else (None, No e1)
            | (None, e2) -> (None, Im (e1,e2))
          end
      end
    | Eq (e1, e2) -> begin
        match getBool e1 tab with
        | (None, e1) -> begin 
            match getBool e2 tab with 
            | (None, e2) -> (None, Eq (e1, e2)) 
            | (Some b, _) -> if b then (None, e1) else (None, No e1) 
          end
        | (Some b, e1) -> begin
            match getBool e2 tab with
            | (None, e2) -> (None, if b then e2 else (No e2))
            | (Some c, _) -> (Some (c=b), T (c = b)) 
          end
      end


  let mk (i : int) (l : node) (k : node) : node =
    if l = k then l
    else
    if member i l k then 
      lookup i l k
    else
      let u = add i l k in
      insert i l k u;
      add_new_node u i;
      let node = (u, Interface.t_to_string !var_tab.(i)) in
      let nodel = (l, "") in (** ici le nom de la node n'a pas d'importance vu que la node existe déjà dans le graphe sous un autre nom **)
      let nodek = (k, "") in (** ici le nom de la node n'a pas d'importance vu que la node existe déjà dans le graphe sous un autre nom **)
      Graph.add_node node;
      Graph.add_node nodel;
      Graph.add_node nodek;
      Graph.add_liaison node nodek;
      Graph.add_liaison node nodel;
      u

  let build (f : int exp) : node = 
    let jkl = ref 0 in
    let tab = Array.init (!n) (function a -> None) in
    let rec aux (f : int exp) (i : int) : node =
      match getBool f tab with
      | (None, nf) -> begin
          if i >= (!n) then failwith "exception de getBool"
          else begin
            let v0 = tab.(i) <- (Some false); aux nf (i+1) in
            let v1 = tab.(i) <- (Some true); aux nf (i+1) in
            tab.(i) <- None;
            mk i v0 v1
          end
        end
      | (Some b, _) -> 
        if b then 
          begin 
            jkl := !jkl + 1; if !jkl mod 100 = 0 then print_endline(string_of_int !jkl);
            Graph.add_node (1, "true"); 1 
          end 
        else 
          begin
            Graph.add_node (0, "false"); 0
          end
    in
    aux f 0


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

  let restrict (u : node) (j : 'a) (b : bool) : node = 
    let rec aux (u : node) : node =
      if Interface.compare !var_tab.(var u) j < 0 then u
      else if Interface.compare !var_tab.(var u) j > 0 then mk (var u) (aux (low u)) (aux (high u))
      else if b then aux (high u)
      else aux (low u)
    in
    aux u

  let satCount (u : node) : int =
    let tmp = Array.of_list (List.rev !node_to_var) in
    let node_of_var = Array.init (Array.length tmp) (fun i -> snd tmp.(i)) in
    let rec aux (u : node)  = 
      if u = 0 then (0,0)
      else if u = 1 then (0,1)
      else
        let ind = node_of_var.(u) in
        let som = 
          let (i,res) = aux (low u) in two_power (ind - i - 1) * res 
                                       +
                                       let (i, res) = aux (high u) in two_power (ind - i - 1) * res
        in (ind, som)
    in
    let size = Array.length !var_tab in
    if u = 0 then 0
    else if u = 1 then two_power size
    else let (i,res) = aux u in two_power (size - i) * res

  (** A modifier pour qu'il ne fasse qu'une partie du build jusqu'à la premiere solution trouvée **)
  let rec anySat (u : node) : (Interface.t * bool) list =
    let rec aux u = 
      if u = 0 then raise (No_SAT "Solution introuvable")
      else if u = 1 then []
      else if low u = 0 then (!var_tab.(var u), true) :: (aux (high u))
      else (!var_tab.(var u), false) :: (aux (low u))
    in
    fill_var 0 (aux u)


  let allSat (u : node) : (Interface.t * bool) list list =
    let rec aux (u : node) (l : (Interface.t * bool) list) (r : (Interface.t * bool) list list) : (Interface.t * bool) list list =
      if u = 0 then r
      else if u = 1 then l::r
      else aux (high u) ((!var_tab.(var u), true)::l) (aux (low u) ((!var_tab.(var u), false)::l) r)
    in 
    List.rev (List.rev_map (fun a -> fill_var 0 a) (aux u [] []))

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



  (************************ Fonctions principales *****************************)

  let main (e : int exp) (i : int) (name : string): unit = 
    n := i; 
    initT();
    initH();
    let x = build e in
    Graph.output_file name;
    aff x;
    print_endline (string_of_int (satCount x))(*; allSat x*)


  let main_expression (e : Interface.t exp) (name : string) : unit = 
    print_endline (string_of_exp e "" Interface.t_to_string);
    print_endline "Start Build ...";
    let exp = (translate_var e) in
    main exp (Array.length !var_tab) name

end
