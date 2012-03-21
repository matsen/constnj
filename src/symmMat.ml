
module SymmAA (N: Number.NUMBER) = 
  struct
    type t = N.t
    type mat = N.t array array
    let create n m = assert(n=m); Array.create_matrix n m N.zero
    let create_and_set n m e = assert(n=m); Array.create_matrix n m e
    let get m i j = m.(i).(j) 
    let set m i j e = m.(i).(j) <- e; m.(j).(i) <- e 
    let n_rows m = Array.length m
    let n_cols m = Array.length m.(0)
    let of_arr_arr m = assert(m<>[||] && n_rows m = n_cols m);m
    let equal = (=)
end

module SM = Mat.Matrix (Number.R) (SymmAA (Number.R))

let make n = SM.create n n
let get = SM.get
let set = SM.set
let print = SM.print
let of_file = SM.of_file

(* note that init only calls f with i <= j, in upper triangle *)
let init n f = 
  let m = make n in
  for i=0 to n-1 do
    for j=i to n-1 do
      set m i j (f i j)
    done
  done;
  m

let assertSymm m = 
  let n = Array.length m in
  Array.iter (
    fun a -> assert(Array.length a = n)
  ) m;
  for i=0 to n-1 do
    for j=i to n-1 do
      if (m.(i).(j) <> m.(j).(i)) then
        failwith (
          Printf.sprintf 
            "matrix not symmetric: %f vs %f" m.(i).(j) m.(j).(i))
    done
  done

let size m = assertSymm m; Array.length m
