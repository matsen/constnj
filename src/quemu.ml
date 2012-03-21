let assertDistMat m = 
  SymmMat.assertSymm m;
  SymmMat.SM.iter (fun x -> x >= 0.) m;
  ()

let muOfDistMat d = 
  assertDistMat d;
  let weight = 1. /. (float_of_int ((Array.length d)-1)) in
  SymmMat.init (Array.length d) (
    fun i j -> weight *. (SymmMat.get d i j)
  )

(* upperTriTotal: total of all strictly upper triangular elements *)
let upperTriTotal m =
  let tot = ref 0. in
  let n = Array.length m in
  for i=0 to n-1 do
    for j=i+1 to n-1 do
      tot := !tot +. SymmMat.get m i j
    done
  done;
  !tot

let qOfMu mu = 
  let r = Array.length mu in
  assert(r > 2);
  let c1 = Base.int_div (r-1) 2
  and c2 = Base.int_div (r-1) (2*(r-2))
  and c3 = Base.int_div 1 (r-2)
  and theta = Array.map (Array.fold_left ( +. ) 0.) mu in
  let tau = upperTriTotal mu in
  SymmMat.init r (
    fun i j -> 
      if i <> j then 
        -. c1*.mu.(i).(j) +. c2*.(theta.(i)+.theta.(j)) -. c3*.tau
      else 0.
  )

(* coalMu: coalesce index1 and index2 in mu. *)
let coalMu mu index1 index2 = 
  let r = Array.length mu in
  let c2 = Base.int_div (r-1) (r-2) in
  let c1 = c2 /. 2. in
  let (s1, s2) = Base.sortPair (index1, index2) in
  (* below: we are eliminating row and column s2 *)
  let boostIndex k = 
    let x = k+(if k >= s2 then 1 else 0) in
    if x >= r then failwith ("coalMu: r is "^(string_of_int r));
    x
  in
  try
  SymmMat.init ((Array.length mu)-1) (
    fun i j ->
      if i = j then 0.
      else if i = s1 then 
        c1 *. (mu.(s1).(boostIndex j) +. mu.(s2).(boostIndex j))
      else if j = s1 then 
        c1 *. (mu.(boostIndex i).(s1) +. mu.(boostIndex i).(s2))
      else
        c2 *. mu.(boostIndex i).(boostIndex j)
  )
  with
    | Invalid_argument s -> failwith ("coalMu: "^(string_of_int s1)^" "^(string_of_int s2))
