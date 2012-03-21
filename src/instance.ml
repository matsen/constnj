(*
 * a "profile" is the set of MAP m-values for all of the obstructions
 * "Obs" is short for obstruction.
 *)
exception IncompleteCoalescence of int

let checkForestIndToObsArr nForests nObs givenForestIndToObsArr = 
  for i=0 to nForests-1 do
    List.iter (
      fun (obsInd, which) ->
        if 0 > nObs || obsInd >= nObs then (
          Printf.printf "problem with constraints. given obsInd of %d while nObs is %d\n"
            obsInd nObs;
            assert(false)
        )
    ) givenForestIndToObsArr.(i)
  done

let obsChoiceListToString l =
  "["^(String.concat "; " (
    List.map (
      fun (ind, which) ->
        Printf.sprintf "(%d, %b)" ind which 
    ) l))^"]"

    (*
# forestIndPairLimitsToForestIndToObsArr 3 [|((0,1),2); ((1,2),2)|];;
- : (int * bool) list array =
[|[(0, false)]; [(1, false); (0, true)]; [(1, true)]|]
     *)
let forestIndPairLimitsToForestIndToObsArr nForests obsPairLimits = 
  let forestIndToObsArr = Array.make nForests [] in
  Array.iteri (
    fun obsIndex ((ti1, ti2), limit) -> 
      assert(ti1 < nForests);
      assert(ti2 < nForests);
      forestIndToObsArr.(ti1) <- (obsIndex, false)::(forestIndToObsArr.(ti1));
      forestIndToObsArr.(ti2) <- (obsIndex, true)::(forestIndToObsArr.(ti2));
  ) obsPairLimits;
  forestIndToObsArr

class instance arg = 
  let nForests, nObs, njForestArr, forestIndToObsArr, obsArr, obsLimits = 
    match arg with
      | `OfData (givenNjForestArr, givenForestIndToObsArr, givenObsArr, givenObsLimits) -> 
          assert(Base.array_allSame (Array.map (fun f -> f#getSize) givenNjForestArr));
          let nForests = Array.length givenNjForestArr in
          assert(nForests = Array.length givenForestIndToObsArr);
          let nObs = Array.length givenObsArr in
          assert(nObs = Array.length givenObsLimits);
          checkForestIndToObsArr (Array.length givenNjForestArr) nObs givenForestIndToObsArr;
          nForests, nObs, givenNjForestArr, 
            givenForestIndToObsArr, givenObsArr, givenObsLimits
      | `OfDMsAndForestIndPairLimits (givenDMArr, givenForestIndPairLimits) -> 
          assert(givenDMArr <> [||]);
          let forestSize = SymmMat.size givenDMArr.(0) in
          Array.iter (fun dm -> assert(forestSize = SymmMat.size dm)) givenDMArr;
          let nForests = Array.length givenDMArr
          and nObs = Array.length givenForestIndPairLimits in
          let newForestIndToObsArr = 
            forestIndPairLimitsToForestIndToObsArr nForests givenForestIndPairLimits in
          checkForestIndToObsArr nForests nObs newForestIndToObsArr;
          nForests, 
          nObs,
          Array.init nForests 
            (fun i -> new NjForest.njForest (`OfDistMat givenDMArr.(i))),
          newForestIndToObsArr,
          Array.init nObs 
            (fun i -> new Obstruction.obstruction (`OfSize forestSize)),
          Array.map snd givenForestIndPairLimits
  in


object(self)

  method getStuff = njForestArr, forestIndToObsArr, obsArr, obsLimits 
  method getNForests = nForests
  method getForest i = 
    if i < 0 || i >= nForests then invalid_arg "getForest: bad index";
    njForestArr.(i)
  method getLength = 
    Array.fold_left (fun subLen t -> subLen +. t#getTau) 0. njForestArr
  method getNjForestArr = njForestArr

  method getNCoalsLeft = 
    Array.fold_left ( + ) 0 (
      Array.map (fun f -> f#nTrees - 3) njForestArr)

  method getTau forestInd = (self#getForest forestInd)#getTau
  method getQ forestInd ind1 ind2 = 
    (self#getForest forestInd)#getQ ind1 ind2

  method getProfile = Array.map (fun obs -> obs#getMAPMVal) obsArr

  method print = 
   Array.iter (fun njf -> njf#print) njForestArr;
    (* Array.iter (fun njf -> njf#printForest) njForestArr; *)
    Array.iter (
      fun l -> 
        print_endline (obsChoiceListToString l)) forestIndToObsArr;
    print_endline "obstructions: ";
    Array.iter (fun obs -> obs#print; print_endline "") obsArr;
    print_string "profile: ";
    Base.printIntArr self#getProfile;
    print_string "limits: ";
    Base.printIntArr obsLimits;
    print_endline "forestIndToObsArr: ";
    Array.iteri (
      fun i obsList ->
        Printf.printf "forest %d: " i;
        List.iter 
          (fun (ind, which) -> Printf.printf "(%d, %b)  " ind which) 
          obsList;
        print_endline "";
    ) forestIndToObsArr

  method printObsArr = 
    Array.iter (fun obs -> obs#print; print_endline "") obsArr;
    (*print_string "profile: ";
    Base.printIntArr self#getProfile;*)

  (* constrainedCoalesceToNew: return an opt instance, which is Some instance if the
   * desired coalescence has profile less than the constraintArr, or None if not. *)
  method constrainedCoalesceToNew whichForest matInd1 matInd2 constraintArr = 
    try
      let newObsOptArr = Array.make nObs None in
      (* coalesce all of the obstructions which use the given forest *)
      List.iter (
    (* ghost forests within obstruction arrays are labeled first by their index,
     * then by a "which" bool, determining which of the GF they are *)
        fun (obsInd, which) ->
          (* below: each tree should appear only once in an obstruction *)
          assert(newObsOptArr.(obsInd) = None);
          newObsOptArr.(obsInd) <- 
            (obsArr.(obsInd))#perhapsCoalesceContractToNew which
              ((self#getForest whichForest)#matrixToForestIndex matInd1)
              ((self#getForest whichForest)#matrixToForestIndex matInd2)
              (Some (constraintArr.(obsInd)));
          if newObsOptArr.(obsInd) = None then 
  (* this coalescence does not fall within the obsLimits, so we have to fail *)
            raise Exit
      ) (forestIndToObsArr.(whichForest));
  (* copy over those obstructions which didn't get modified by the tree coal *)
    for obsInd=0 to nObs-1 do
      if newObsOptArr.(obsInd) = None then
        newObsOptArr.(obsInd) <- Some obsArr.(obsInd)
    done;
    let getSome = function | Some x -> x | None -> assert(false) in
    Some (
      new instance (`OfData (
        Array.init nForests (
          fun i -> 
            if i = whichForest then 
              (njForestArr.(i))#matCoalesceToNew matInd1 matInd2
            else njForestArr.(i)),
        forestIndToObsArr, 
        Array.map getSome newObsOptArr, 
        obsLimits)))
    with
      | Exit -> None

  (* perhapsCoalesceToNew: 
   * as above but the constraints are just the obsLimits
   *)
  method perhapsCoalesceToNew whichForest matInd1 matInd2 = 
    self#constrainedCoalesceToNew whichForest matInd1 matInd2 obsLimits

  method getCoals = 
    let indices = ref [] in
    for forestInd=0 to nForests-1 do
      let nTrees = (self#getForest forestInd)#nTrees in
      if nTrees > 3 then (
        indices := (
          List.map (
            fun (ind1, ind2) -> (forestInd, ind1, ind2)
          ) (Base.getUpperTriIndices nTrees)
        )::!indices;
      )
    done;
    List.flatten !indices

  method sortedCoals = 
    List.sort (
      fun (aForestInd, aInd1, aInd2) (bForestInd, bInd1, bInd2) ->
            - (compare (self#getQ aForestInd aInd1 aInd2)
                       (self#getQ bForestInd bInd1 bInd2)))
      (self#getCoals)

  method makeStep = 
    (* below: +1 so that we can achieve the limits *)
    let step = new Tensor.tensor (`Make (Array.map (( + ) 1) obsLimits, None)) 
    and ourProfile = self#getProfile
    in
    let rec aux = function
      | (whichForest, ind1, ind2)::rest -> (
          match self#perhapsCoalesceToNew whichForest ind1 ind2 with
            | Some newCoal -> 
                (* this coalescence satisfies the limits *)
                let newProfile = newCoal#getProfile in
                assert(step#isLegalMIndex newProfile);
                if step#get newProfile = None then 
                  (* this is the first time we've seen this profile *)
                  step#set newProfile (Some newCoal);
                (* next, recur if we haven't found the coal which doesn't increase
                 * the map profile at all. each step we want to find this coal. *)
                if newProfile <> ourProfile then aux rest
                (* try again if not within bounds *)
            | None -> aux rest 
        )
      | [] -> ()
    in
    aux self#sortedCoals;
    step

  method toObtreeArr = 
    Array.mapi (
      fun forestInd njF -> 
        if njF#nTrees > 3 then (self#print; flush_all (); raise (IncompleteCoalescence forestInd));
        njF#toObtree
    ) njForestArr


end

let njForestsEqual i1 i2 = 
  let a1 = i1#getNjForestArr
  and a2 = i2#getNjForestArr in
  if Array.length a1 <> Array.length a2 then false
  else (
    try
      for i=0 to (Array.length a1)-1 do
        if not (NjForest.equal a1.(i) a2.(i)) then
          raise Exit
      done;
      true
    with
      | Exit -> false
  )

(* *** testing functions *** *)

let doXNJs origInst x = 
  let rec aux inst coalsLeft = 
    inst#print;
    if coalsLeft > 0 then (
      match inst#sortedCoals with
        | (whichForest, mind1, mind2)::rest ->
            Printf.printf "\n>>>> coalescing %d and %d of %d...\n" mind1 mind2 whichForest;
            aux (inst#coalesceToNew whichForest mind1 mind2)
                (coalsLeft - 1)
        | [] -> inst
    )
    else inst
  in
  aux origInst x

let linearOfDMList dmList maxDist = 
  let dms = Array.of_list dmList in
  let limits = Array.init ((Array.length dms) - 1) 
                 (fun i -> ((i, i+1), maxDist)) in
  new instance (`OfDMsAndForestIndPairLimits (dms, limits))

let linearOfFiles fnameList maxDist = 
  linearOfDMList 
    (List.map (
       fun fname ->
         snd (ParseDM.dmAndNamesOfPhylipFile fname))
       fnameList)
    maxDist

    (*
     
let i = linearOfFiles ["test/test1.phymat"; "test/test2.phymat"; "test/test1.phymat";] 2;;  
let j = doXNJs i 5;;
let s = j#makeStep;;
     *)
