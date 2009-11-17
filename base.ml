let roundingErrorTolerance = 1e-12

let list_init n f = 
  let rec aux i = if i < n then (f i)::(aux (i+1)) else [] in
  aux 0

(* toNList: lists 0,...,n *)
let toNList n = list_init (n+1) (fun i -> i)

  (* compute all pairs from the two lists *)
let allPairs l1 l2 = 
  List.flatten (
    List.map (
      fun x1 ->
        List.map ( fun x2 -> (x1, x2) ) l2
    ) l1
  )

let hashtbl_toPairs h = 
  Hashtbl.fold (
    fun k v pairList ->
      (k, v) :: pairList
  ) h []

let rec randomCoal n = 
  let i, j = ( Random.int n, Random.int n ) in
  if i = j then randomCoal n
  else if i < j then (i, j)
  else (j, i)

let optMin optA optB = 
  match (optA, optB) with
    | (Some x, Some y) -> Some(min x y)
    | (Some x, None) -> Some(x)
    | (None, Some y) -> Some(y)
    | (None, None) -> None

let sortPair = 
  fun (x,y) -> if x < y then (x, y) else (y, x)

let testBreak () = 
  List.iter (
    fun x ->
      try
        if x = 3 then raise Exit
        else Printf.printf "%d\n" x
      with
        | Exit -> ()
  ) [1;2;3;4;5]

let list_max l =
  assert(l <> []);
  List.fold_left max (List.hd l) (List.tl l)

let matrix_init f rows cols = 
  let m = Array.make_matrix rows cols (f 0 0) in
  for i=0 to rows-1 do 
    for j=0 to cols-1 do 
      m.(i).(j) <- (f i j)
    done;
  done;
  m

let rec atob_list a b = 
  if a > b then []
  else a::(atob_list (a+1) b)

let list_intersect l1 l2 = 
  List.fold_left (
    fun soFar x ->
      if List.mem x l2 then x::soFar else soFar
  ) [] l1

let rec listOverOneRests f = function
  | x::l -> (f x l)@(listOverOneRests f l)
  | [] -> []

(* kTuplesOfList: 
# kTuplesOfList [1;2;3;4] 2;;  
- : int list list = [[1; 2]; [1; 3]; [1; 4]; [2; 3]; [2; 4]; [3; 4]]
 * *)
let rec kTuplesOfList l k = 
  let attach x = List.map (fun lp -> x::lp) in
  if k > 0 then
    listOverOneRests (
      fun x lp ->
        attach x (kTuplesOfList lp (k-1))
    ) l
  else [[]]

(* iterOverKTuplesOfList: 
 * *)
let iterOverKTuplesOfList f l k = 
  let rec aux kRemaining listSoFar availableElts = 
    assert(kRemaining >= 0);
    if kRemaining = 0 then f listSoFar
    else
      match availableElts with
        | x::restAvail -> 
            (* first add to the tuple *)
            aux (kRemaining-1) (x::listSoFar) restAvail;
            (* then forget about it and move on *)
            aux kRemaining listSoFar restAvail
        | [] -> ()
  in
  aux k [] l

let altKTuplesOfList l k = 
  let tuples = ref [] in
  let addTuple t = tuples := t :: (!tuples) in
  iterOverKTuplesOfList addTuple l k;
  !tuples

let print_int_list l = 
  print_string ("{"^(String.concat "; " (List.map string_of_int l))^"} ")

let sortPair = function (x,y) -> if x < y then (x,y) else (y,x)

let list_iterOverAllPairs f l1 l2 = 
  List.iter (fun i -> List.iter (fun j -> f i j) l2) l1

let int_div x y = 
  assert(y <> 0);
  (float_of_int x) /. (float_of_int y)

let list_uniformSelection l = 
  assert (l <> []);
  List.nth l (Random.int (List.length l))

let intArrToString a = 
  "[|"^(String.concat "; " (List.map string_of_int (Array.to_list a)))^"|]"

let intArrToStringShort a = 
  String.concat ";" (List.map string_of_int (Array.to_list a))

let printIntArr a = print_endline (intArrToString a)

let allNone a =
  Array.fold_left (
    fun soFar status ->
      match status with
        | Some x -> false
        | None -> soFar
  ) true a

let pairOfTwoList l = 
  assert(List.length l = 2);
  (List.nth l 0, List.nth l 1)

let pairsOfList l = List.map pairOfTwoList (kTuplesOfList l 2)

let extractSomes optList = 
  List.fold_left (
    fun l -> function
      | Some x -> x::l
      | None -> l
  ) [] (List.rev optList)

(* the following functions allow the user to supply a comparison function *)
let list_memCompare compareFun x l = 
  List.fold_left (
    fun soFar elt ->
      soFar || (compareFun x elt = 0)
  ) false l

let list_uniquesCompare compareFun linit = 
  List.rev (
    List.fold_left (
      fun l x ->
        if list_memCompare compareFun x l then l 
        else x :: l
    ) [] linit )

let list_uniques l = list_uniquesCompare compare l

let string_toFile fname str = 
  let ch = open_out fname in
  Printf.fprintf ch "%s\n" str;
  close_out ch

(*
# basename "/home/matsen/distrec/ocaml/base.ml";;
- : string = "base"
 *)

let basename str = 
  Pcre.replace ~pat:"(?:[^\\/]*\\/)*" (
    Pcre.replace ~pat:"\\.[^\\.]+$" str)

let rec list_powerSet = function
  | x::l ->
     let lp = list_powerSet l in
     lp @ (List.map (fun set -> x::set) lp)
  | [] -> [[]]

let timeStrOfSeconds secs = 
  let iSecs = int_of_float secs in
  let hours = iSecs / 3600 in
  let mins = iSecs / 60 - 60*hours in
  Printf.sprintf "%02d:%02d:%02.1f" hours mins
    (secs -. (float_of_int (hours*3600)) -. (float_of_int (mins*60)))

(* getUpperTriIndices:
 * get the indices of an upper triangular matrix. 
 *)
let rec getUpperTriIndices n = 
  if n <= 0 then []
  else 
    (List.map (fun i -> (i, n-1)) (toNList (n-2)))@(
      getUpperTriIndices (n-1))

let array_allSame a =
  if a = [||] then true
  else 
    try
      Array.iter (
        fun x -> 
          if x <> a.(0) then raise Exit
      ) a;
      true
    with
      | Exit -> false

  (* skips comments *)
let stringListOfFile fname = 
  let lines = ref [] in
  let chIn = open_in fname in
  try 
    while true do 
      let line = input_line chIn in
      if not (Pcre.pmatch ~pat:"^#" line) then
        lines := line :: !lines
    done;
    List.rev !lines
  with 
  End_of_file -> (List.rev !lines)

let stringListToFile fname stringList = 
  let ch = open_out fname in
  List.iter (
    fun s -> Printf.fprintf ch "%s\n" s
  ) stringList;
  close_out ch

let splitOnSpace str = 
  List.filter (( <> ) "") (Pcre.split ~pat:"\\s+" str)

let array_map2 f a b = 
  let n = Array.length a in
  if n <> Array.length b then
    failwith "array_map2 called with arrays of different lengths!";
    Array.init n (fun i -> f a.(i) b.(i))

let list_uniques linit = 
  List.rev (
    List.fold_left (
      fun l x ->
        if List.mem x l then l 
        else x :: l
    ) [] linit )

let makeRng seed = 
  let rng = Gsl_rng.make Gsl_rng.MT19937 in
  Gsl_rng.set rng (Nativeint.of_int seed);
  rng

let isOKFloat x = 
  let c = classify_float x in
  c <> FP_infinite && c <> FP_nan

let getSome = function
  | Some x -> x
  | None -> failwith "getSome: none!"

(* think of None as being greater than any Some *)
let optCompare compareFun o1 o2 = 
  match (o1, o2) with
    | (Some i1, Some i2) -> compareFun i1 i2
    | (Some i, None) -> (-1)
    | (None, Some i) -> 1
    | (None, None) -> 0

let array_argMin compareFun a = 
  assert(a <> [||]);
  let bestI = ref 0 in
  for i=1 to (Array.length a)-1 do
    if compareFun a.(!bestI) a.(i) > 0 then bestI := i
  done;
  !bestI

let array_min compareFun a = 
  a.(array_argMin compareFun a)

let list_avgFloats l = 
  (List.fold_left ( +. ) 0. l) /. (float_of_int (List.length l))

(* checkEach: make sure that f evaluates to true on each element of l *)
let checkEach f l = List.fold_left (fun soFar x -> soFar && (f x)) true l

