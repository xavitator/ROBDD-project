open Implementation

let rec shift p v i =
  if i < Array.length p then begin
    p.(i) <- v; shift p (v + 1) (i + 1) end

let rec moveLast (p : int array) (s : int) (i : int) =
  let size_p = Array.length p in
  if i < 0 then false
  else if p.(i) >= (s - 1) then moveLast p s (i-1)
  else if i >= (size_p - 1) then begin p.(i) <- p.(i) + 1; true end
  else if p.(i + 1) = p.(i) + 1 then moveLast p s (i - 1)
  else begin shift p (p.(i) + 1) i; true end

let add_sol ens p res =
  let list_init n f =
    let rec add_el i l =
      if(i < 0 || i >= n) then l
      else add_el (i-1) ((f i) :: l)
    in
    add_el (n-1) []
  in
  res := (list_init (Array.length p) (fun a -> ens.(p.(a)))) :: (!res)

let k_combinaison k ens =
  let n = Array.length ens in
  if k <= 0 || k > n  then []
  else
    let res = ref [] in
    let p = Array.init k (fun a -> a) in
    let rec aux () =
      begin
        add_sol ens p res;
        if(moveLast p (Array.length ens) (Array.length p - 1)) then aux ()
        else !res
      end
    in
    aux ()

let comb_to_tab k n =
  let ens = Array.init n (fun i -> i) in
  let l = k_combinaison k ens in
  let rec complete_tab tmp l =
    match l with
    | [] -> tmp
    | h :: q -> begin
        let arr = Array.init n (fun i -> false) in
        let rec loop li = 
          match li with
          | [] -> ()
          | t :: s -> arr.(t) <- true; loop s
        in
        loop h; complete_tab (arr :: tmp) q
      end
  in
  complete_tab [] l

let create_conj arr = 
  let rec aux i =
    if i >= Array.length arr - 1 then if arr.(i) then Var i else No (Var i)
    else Et( (if arr.(i) then Var i else No (Var i)) , aux (i+1))
  in
  aux 0

let create k n = 
  let rec loop tmp l =
    match l with
    | [] -> (match tmp with None -> T false | Some t -> t)
    | h :: q -> begin match tmp with
        | None -> loop (Some (create_conj h)) q
        | Some s -> loop (Some (Ou (s, create_conj h))) q
      end
  in
  loop None (comb_to_tab k n)