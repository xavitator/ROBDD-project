open Implementation;;
(*p(k,i,j) -> numero k en (i,j) -> var = k+i*n+j*n*n 0 <= i, j, k, <= !nb*)

let n : int ref = ref 0;;

let nb : int ref = ref 0;;

(*let initn (i : int) : unit = n := i; nb := i*i*)

(*let v (x : int) : (int*int*int) = let (k,l) = (x mod (!nb),x/(!nb)) in let (i,j) = (l mod (!nb),l/(!nb)) in (k,i,l)*)

(*^[0<=i<=8]^[0<=j<=8](v[0<=k<=8]p(k,i,j)^^[0<=k<=8](^[0<=u<=8;0<=v<=8;(u,v)!=(i,j)](-p(k,i,j)v-p(k,u,v))^^[...](-p(k,i,j)v-p(k,u,v))))*)

let sudoku n =
  let nb = n*n in
  let p k i j = k + i*nb + j*nb*nb in
  let rec eti i =
    let rec etj j =
      let rec ouk k =
        let e = Var (p k i j) in
        if k = nb-1 then e
        else Ou(e, ouk (k+1))
      in
      let rec etk k =
        let rec eti2 i2 =
          if i = i2 then if i = nb-1 then T true else eti2 (i2+1) else
            let e = No(Var (p k i2 j)) in
            if i2 = nb-1 then e
            else Et(e, eti2 (i2+1))
        in
        let rec etj2 j2 =
          if j = j2 then if j = nb-1 then T true else etj2 (j2+1) else
            let e = No(Var (p k i j2)) in
            if j2 = nb-1 then e
            else Et(e, etj2 (j2+1))
        in
        let rec eti3 i3 =
          let rec etj3 j3 =
            if i = i3 && j = j3 then if j = n*(j/n+1)-1 then T true else etj3 (j3+1) else
              let e = No(Var (p k i3 j3)) in
              if j3 = n*(j/n+1)-1 then e
              else Et(e, etj3 (j3+1))
          in
          let e = etj3 (n*(j/n)) in
          if i3 = n*(i/n+1)-1 then e
          else Et(e, eti3 (i3+1))
        in
        let e = Im(Var (p k i j) ,Et(Et(eti2 0, etj2 0), eti3 (n*(i/n)))) in
        if k = nb-1 then e
        else Et(e, etk (k+1))
      in
      let e = Et(ouk 0, etk 0) in
      if j = nb-1 then e
      else Et(e, etj (j+1))
    in
    let e = etj 0 in
    if i = nb-1 then e
    else Et(e, eti (i+1))
  in eti 0


let main_sudo n = (*initn n;*) main (sudoku n) (n*n*n*n*n*n) (string_of_int n ^ "_sudoku.dot")
(*sudoku 1 -> Et(Var 0, Et(T true, T true))*)
(*sudoku 2 -> 288 solutions*)
let main3 = main (Et(Et(Var 0,Et(Var 10,Et(Var 20,Et(Var 30,Et(Var 40,Et(Var 50,Et(Var 60,Et(Var 70,Var 80)))))))),sudoku 3)) (3*3*3*3*3*3) ("3_sudoku.dot")
let _ = main3












