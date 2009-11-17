type relationship = Greater | Less | Equal | Incomparable

let mIndCompare mInd1 mInd2 = 
  let diff = Base.array_map2 ( - ) mInd1 mInd2 in
  Array.fold_left (
    fun soFar elt ->
      match soFar with
        | Greater -> if elt >= 0 then Greater else Incomparable
        | Less -> if elt <= 0 then Less else Incomparable
        | Equal -> 
            if elt > 0 then Greater
            else if elt < 0 then Less
            else Equal
        | Incomparable -> Incomparable
  ) Equal diff

(* inefficient!
# let m = findMinElts [ [|1;0;0|]; [|0;0;1|]; [|0;1;0|]; ];;
val m : int array list = [[|1; 0; 0|]; [|0; 0; 1|]; [|0; 1; 0|]]
# let m = findMinElts [ [|1;0;0|]; [|0;0;0|]; [|0;1;0|]; ];;
val m : int array list = [[|0; 0; 0|]]
* *)
let findMinElts eltList = 
  let mins = ref (Base.list_uniques eltList) in
  List.iter (
    fun elt -> 
      mins := 
        List.filter (
          (* eliminate those elements which are greater than us *)
          fun another -> not (mIndCompare another elt = Greater)
        ) !mins
  ) eltList;
  !mins

(*
# positiveOneLessList [|1;2;0|];;
- : int array list = [[|1; 1; 0|]; [|0; 2; 0|]]
*)
let positiveOneLessList mInd = 
  let l = ref [] in
  let perhapsAdd x = 
    if not (List.mem x !l) then l := x::(!l)
  in
  for ind=0 to (Array.length mInd)-1 do
    if mInd.(ind) > 0 then
      perhapsAdd (
        Array.mapi 
          (fun i y -> if i = ind then y-1 else y) mInd)
  done;
  !l

(* lists mInds which would be an improvement by one index over one of the
 * minimal mInds from the list
# let m = improvementMInds [ [|1;1;1|]; [|1;0;2|]; [|0;1;0|]; ];;
val m : int array list = [[|1; 0; 1|]; [|0; 0; 2|]; [|0; 0; 0|]]
# let m = improvementMInds [ [|0;0;2|]; [|0;1;0|]; ];;
val m : int array list = [[|0; 0; 1|]; [|0; 0; 0|]]
* *)

let improvementMInds mIndList = 
  Base.list_uniques (
    List.flatten (
      List.map positiveOneLessList (findMinElts mIndList)))

(*
 * mustDoBetterThan: 
 * takes the element-wise maximum of the collection of improvementMInds.
 * any improvement mInd must be less than or equal to this elt wise maximum.
# let m = mustDoBetterThan [ [|1;1;1|]; [|1;0;2|]; [|0;1;0|]; ];;
val m : int array = [|1; 0; 2|]
# let m = mustDoBetterThan [ [|0;0;2|]; [|0;1;0|]; ];;
val m : int array = [|0; 0; 1|]
 *)
let mustDoBetterThan mIndList = 
  let improvements = improvementMInds mIndList in
  assert(improvements <> []);
  List.fold_left (Base.array_map2 ( max ))
    (List.hd improvements) (List.tl improvements)

let notGreaterThanList mInd mIndList = 
  List.fold_left (
    fun soFar fromList ->
      soFar && ((mIndCompare mInd fromList) <> Greater)
  ) true mIndList



(*
let m = improvementMInds [ [|0; 1; 1|]; [|1; 0; 1|]; [|1; 1; 0|]; ];;
let m = mustDoBetterThan [ [|0; 1; 1|]; [|1; 0; 1|]; [|1; 1; 0|]; ];;
let m = mustDoBetterThan [ [|1; 2; 1|]; [|0; 2; 1|]; [|0; 1; 0|] ];;
 *)

