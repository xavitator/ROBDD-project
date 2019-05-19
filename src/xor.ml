open Implementation

let create n =
  let rec loop i = 
    if i >= n then Var (n-1)
    else 
      let tmp = loop (i+1) in
      let v = Var (i-1) in
      Et(Im(v, No tmp), Im(tmp, No v))
  in
  loop 1