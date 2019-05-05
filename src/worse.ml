open Implementation

let all_permut v =
  let vars = Array.copy v in
  let permut vars i =
    if i > 0 && i < Array.length vars then 
      let tmp = vars.(i-1) in
      vars.(i-1) <- vars.(i); vars.(i) <- tmp; true
    else false
  in
  let equi t1 t2 =
    if Array.length t1 <> Array.length t2 then false
    else 
      let rec aux i n =
        if i >= n || i < 0 then true
        else 
        if t1.(i) <> t2.(i) then false
        else aux (i+1) n
      in
      aux 0 (Array.length t1)
  in
  let rec gen l i =
    if i <= 0 && equi vars v then l
    else if permut vars i then 
      gen ((Array.copy vars) :: l) (i-1)
    else
      gen l (Array.length vars - 1) 
  in
  gen [] (Array.length vars - 1)

let create_opposite e =
  let rec aux e =
    match e with
    | Var _ -> No (e)
    | No _ -> e
    | Et (e1, e2) -> Et(aux e1, aux e2)
    | T b -> T (not b)
    | _ -> failwith "create_opposite -> ne devrait pas arriver"
  in
  aux e

let create_conj vars =
  let rec aux i e = 
    if i < 0 || i >= Array.length vars then 
      match e with
      | None -> T false
      | Some e -> e
    else
      let v = if i mod 2 = 0 then Var vars.(i) else No (Var vars.(i)) in
      match e with
      | None -> aux (i+1) (Some v)
      | Some e -> aux (i+1) (Some (Et(e, v)))
  in
  aux 0 None

let create_disj l =
  let rec aux l res =
  match l with
  | [] -> res
  | h :: q -> begin
    match res with 
    | None -> aux q (Some h)
    | Some res -> aux q (Some(Ou(h, res)))
  end
  in
  match aux l None with
  | None -> T true
  | Some res -> res

let worse n = 
  assert (n > 0);
  let vars = Array.init n (fun a -> a) in
  let all_p = all_permut vars in
  let conj = List.rev_map (fun a -> create_conj a) all_p in
  let conj_op = List.rev_map (fun a -> create_opposite a) conj in
  (* create_disj (List.rev_append conj conj_op) *)
  create_disj conj 

