open Implementation

let one_per_line n =
  let rec line i tmp =
    let rec col j tmp =
      if j >= n then tmp 
      else
        match tmp with
        | None -> col (j+1) (Some (Var (i * n + j)))
        | Some e -> col (j + 1) (Some (Ou (Var (i * n + j), e)))
    in
    if i >= n then tmp
    else
      let c = col 0 None in
      match tmp with
      | None -> line (i+1) c
      | Some e -> 
        begin
          match c with
          | None -> line (i + 1) tmp 
          | Some a -> line (i + 1) (Some (Et (a, e)))
        end
  in
  line 0 None

let create_exp i j n =
  let rec restrict_line a tmp =
    if a >= n then tmp
    else if a = j then restrict_line (a + 1) tmp
    else
      match tmp with
      | None -> restrict_line (a + 1) (Some (No (Var (i * n + a))))
      | Some e -> restrict_line (a + 1) (Some (Et (No (Var (i * n + a)), e)))
  in
  let rec restrict_col a tmp =
    if a >= n then tmp
    else if a = i then restrict_col (a + 1) tmp
    else
      match tmp with
      | None -> restrict_col (a + 1) (Some (No (Var (a * n + j))))
      | Some e -> restrict_col (a + 1) (Some (Et (No (Var (a * n + j)), e)))
  in
  let rec restrict_diagD l c tmp =
    if l <0 || c < 0 || l >= n || c >= n then tmp
    else if c = j && l = i then restrict_diagD (l - 1) (c + 1) tmp
    else
      match tmp with
      | None -> restrict_diagD (l - 1) (c + 1) (Some (No (Var (l * n + c))))
      | Some e -> restrict_diagD (l - 1) (c + 1) (Some (Et (No (Var (l * n + c)), e)))
  in
  let rec restrict_diagM l c tmp =
    if l <0 || c < 0 || l >= n || c >= n then tmp
    else if c = j && l = i then restrict_diagM (l + 1) (c + 1) tmp
    else
      match tmp with
      | None -> restrict_diagM (l + 1) (c + 1) (Some (No (Var (l * n + c))))
      | Some e -> restrict_diagM (l + 1) (c + 1) (Some (Et (No (Var (l * n + c)), e)))
  in
  let line = match restrict_line 0 None with 
    | None -> T true
    | Some e -> e
  in
  let col = match restrict_col 0 None with 
    | None -> T true
    | Some e -> e
  in
  let el = min j (n - 1 - i) in
  let diagD = match restrict_diagD (i + el) (j - el) None with 
    | None -> T true
    | Some e -> e
  in
  let el = min i j in
  let diagM = match restrict_diagM (i - el) (j - el) None with 
    | None -> T true
    | Some e -> e
  in
  let res = Et (Et (line, col), Et(diagD, diagM)) in
  Im (Var (i * n + j), res)

let create_all n =
  let rec aux ind tmp =
    if ind >= n * n then tmp
    else 
      let res = create_exp (ind / n) (ind mod n) n in
      match tmp with
      | None -> aux (ind + 1) (Some res)
      | Some e -> aux (ind + 1) (Some (Et (res, e)))
  in
  let impl = match aux 0 None with
    | None -> T true
    | Some e -> e
  in
  let one = match one_per_line n with
    | None -> T true
    | Some e -> e
  in
  Et (one, impl)

let main_queen n = main (create_all n) (n * n)

let _ = main_queen 8
