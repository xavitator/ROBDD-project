open Implementation

let all_permut v =
  let vars = Array.to_list v in
  let insert_i l a i = 
    let arr = Array.make (List.length l + 1) a in
    List.iteri (fun j b -> arr.(j + (if j < i then 0 else 1)) <- b) l; 
    Array.to_list arr
  in
  let rec permut vars =
    match vars with
    | [] -> []
    | h :: [] -> [[h]]
    | h :: q -> 
      let aux a b =
        let size = List.length b in
        let rec loop i res =
          if i > size then res
          else loop (i+1) ((insert_i b h i) :: res)
        in
        loop 0 a
      in
      List.fold_left aux [] (permut q)
  in
  let res = permut vars in
  List.map Array.of_list res


let create_opposite e =
  let rec aux e =
    match e with
    | Var _ -> No (e)
    | No _ -> e
    | Et (e1, e2) -> Et(aux e1, aux e2)
    | Ou (e1, e2) -> Ou(aux e1, aux e2)
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
      | Some e -> aux (i+1) (Some (Ou(e, v)))
  in
  aux 0 None

let create_disj l =
  let rec aux l res =
    match l with
    | [] -> res
    | h :: q -> begin
        match res with 
        | None -> aux q (Some h)
        | Some res -> aux q (Some(Et(h, res)))
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
  create_disj (List.rev_append conj conj_op)
(* create_disj conj  *)

