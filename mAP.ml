(*
 * mAP.ml: maximum agreement partition 
 * 
 * notes:
 * the MVal is the m-value, i.e. one less than the size of the agreement
 * partition
 *)

(* set the max number of recursions to find the MAP *)
let maxMAPMVal = 10 

(* partitionMakesCompatible: 
 * check if a given partition cuts the rsplits so that they are compatible.
 *)
let partitionMakesCompatible partition splitList1 splitList2 = 
  let prepInter y splitList = 
    List.filter (fun inter -> not (Rsplit.IntSet.is_empty inter)) (
      List.map (fun s -> Rsplit.IntSet.inter y s) splitList)
  in
  try 
    List.iter (
      fun y ->
        List.iter (
          fun s1 ->
            List.iter (
              fun s2 -> if not (Rsplit.compatible s1 s2) then raise Exit
            ) (prepInter y splitList2)
        ) (prepInter y splitList1)
    ) (Rsplit.intSetListOfPartition partition);
    true
  with
    | Exit -> false

let ccComponentsAreSame partition gf1 gf2 = 
  GhostForestFuns.getCCComponents partition gf1 = 
    GhostForestFuns.getCCComponents partition gf2

let partitionIsAP partition gf1 gf2 = 
  (gf1#convexWRTPartition partition) && (gf2#convexWRTPartition partition)
  && (partitionMakesCompatible 
        partition gf1#toRelevantEdgeRsplitList gf2#toRelevantEdgeRsplitList)
  && ccComponentsAreSame partition gf1 gf2

let verbosePartitionIsAP partition gf1 gf2 = 
  print_endline "";
  print_string "the part'n: ";
  Base.printIntArr partition;
  if not (gf1#convexWRTPartition partition) then (
    print_endline "not convex wrt gf1"; false
  )
  else if not (gf2#convexWRTPartition partition) then (
    print_endline "not convex wrt gf2"; false
  )
  else if not (
    partitionMakesCompatible 
      partition gf1#toRelevantEdgeRsplitList gf2#toRelevantEdgeRsplitList) then (
        print_endline "doesn't cut into an agreement part'n"; false
      )
  else if not (ccComponentsAreSame partition gf1 gf2) then (
    print_endline "CC components not the same!"; false
  )
  else
    true


(* findOfMVal:
 * return Some MAP of m-value m if it exists, otherwise None. 
 * *)
let findOfMVal gf1 gf2 m = 
  let nTaxa = gf1#getSize in
  assert(nTaxa = gf2#getSize);
(* precalculate the edge rsplits rather than use partitionIsAP to save a little
 * time: *)
  let relevRs1 = gf1#toRelevantEdgeRsplitList
  and relevRs2 = gf2#toRelevantEdgeRsplitList in
  (* use the most resolved tree for speed *)
  let (useMySplitsGF, otherGF) = 
    if gf1#nGhosts <= gf2#nGhosts then (gf1, gf2) else (gf2, gf1)
  in
  let isGoodPartition partition = 
    (* partition made from splits of a forest must be convex wrt it *)
    (* assert(useMySplitsGF#convexWRTPartition partition); *) (* DBG *)
    otherGF#convexWRTPartition partition &&
    partitionMakesCompatible partition relevRs1 relevRs2 &&
    ccComponentsAreSame partition gf1 gf2
  in
  (* below: we want the partition to be of size one more than the m-value *)
  useMySplitsGF#findGoodMPartition isGoodPartition m



(* findOfMVal:
 * return Some MAP of m-value m if it exists, otherwise None. 
 * *)
let oldFindOfMVal gf1 gf2 m = 
  let nTaxa = gf1#getSize in
  assert(nTaxa = gf2#getSize);
(* precalculate the edge rsplits rather than use partitionIsAP to save a little
 * time: *)
  let relevRs1 = gf1#toRelevantEdgeRsplitList
  and relevRs2 = gf2#toRelevantEdgeRsplitList in
  (* use the most resolved tree for speed *)
  let (useMySplitsGF, otherGF) = 
    if gf1#nGhosts <= gf2#nGhosts then (gf1, gf2) else (gf2, gf1)
  in
  let allRsplits = useMySplitsGF#toAllRsplits in
  let isGoodPartition partition = 
    (* partition made from splits of a forest must be convex wrt it *)
    (* assert(useMySplitsGF#convexWRTPartition partition); *) (* DBG *)
    otherGF#convexWRTPartition partition &&
    partitionMakesCompatible partition relevRs1 relevRs2 &&
    ccComponentsAreSame partition gf1 gf2
  in
  (* below: we want the partition to be of size one more than the m-value *)
  Rsplit.findGoodMPartition isGoodPartition nTaxa m allRsplits
    (* couls use useMySplitsGF#findGoodMPartition m isGoodPartition *)

(* findWithinLimits: return Some partition only if that partition has m
 * such that mLower <= m <= optMUpper (if optMUpper has a value).
*)
let rec findWithinLimits gf1 gf2 mLower optMUpper = 
  (* copy then out identical trees *)
  let cf1 = gf1#copy
  and cf2 = gf2#copy in
  (* isomCluster is the set of indices of identical trees. we treat them as a
   * group *)
  let isomCluster = ref [] in
  let i = ref cf1#getSize in
  while !i > 0 do
    i := !i-1;
    match (cf1#getGhostOpt !i, cf2#getGhostOpt !i) with
      | (Some g1, Some g2) -> if g1 = g2 then isomCluster := (!i)::!isomCluster
      | (Some g1, None) -> ()
      | (None, Some g2) -> ()
      | (None, None) -> ()
  done;
  let rec aux m = 
    match findOfMVal cf1 cf2 m with
      | Some partition -> Some partition
      | None ->
          (* no partition of MValue m *)
          match optMUpper with
            | Some mUpper -> if m < mUpper then aux (m+1) else None
            | None ->  (* no specified upper bound *)
                if m < maxMAPMVal then aux (m+1) else None
  in
  if List.length !isomCluster > 1 then (
    (* we have some trees to eliminate *)
    let representative = List.hd !isomCluster in
    List.iter (
      fun i -> 
        cf1#setGhostOpt i None;
        cf2#setGhostOpt i None;
    ) (List.tl !isomCluster); (* don't eliminate the representative! *)
    match aux mLower with
      | Some partition -> 
          (* extend the partition to all of the isomCluster from the representative *)
          List.iter (fun i -> partition.(i) <- partition.(representative)) 
            (List.tl !isomCluster);
          assert(partitionIsAP partition gf1 gf2); (* perhaps cut later? not a big slowdown *)
          Some partition
      | None -> None
  )
  else aux mLower

(* find: 
 * find a MAP *)
let rec find gf1 gf2 = 
  match findWithinLimits gf1 gf2 0 None with
    | Some partition -> partition
    | None -> failwith ("SPR distance greater than "^(string_of_int maxMAPMVal))
  
(* *** testing *** *)


let yuleTest n k = 
  let gf1 = GhostForestFuns.partialYule n k
  and gf2 = GhostForestFuns.partialYule n k in
  gf1#print;
  gf2#print;
  find gf1 gf2

let yuleTreeTest n = yuleTest n (n-1)

(*
let gf1 = GhostForestFuns.partialYule 8 5;;
let gf2 = GhostForestFuns.partialYule 8 6;;
gf1#print;;
gf2#print;;
let p = find gf1 gf2;;


let o1 = Obtree.ofNewick "(0,(1,(2,(3,(4,(5,6))))))";;
let o2 = Obtree.ofNewick "(0,(6,(5,(4,(3,(1,2))))))";;
let (gf1, gf2) = GhostForestFuns.gfOfObtreePair o1 o2;;

let p = findOfMVal gf1 gf2 3;;

let part = [|0; 1; 1; 2; 2; 3; 3|];;

partitionIsAP part gf1 gf2;;

let r1 = gf1#toAllRsplits;;
let r2 = gf2#toAllRsplits;;
let l = List.map (List.nth r1) [9;5;1];;                         
let desired = Rsplit.infimum 7 l;;
let pre = Rsplit.IntSetSet.elements (Rsplit.preInfimum 7 l);;

let x = Rsplit.findGoodMPartition ((=) desired) 7 3 r1;;
let x = Rsplit.findGoodMPartition (fun p -> p.(0) = 0) 7 3 r1;;

let ps = Rsplit.findAllMPartitions 7 3 r1;;
*)
