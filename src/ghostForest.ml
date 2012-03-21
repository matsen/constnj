(* ghostForest.ml
 *
 *)

exception GoodPartition of int array

module IS = Rsplit.IntSet

class ghostForest arg = 

  let size, taxonToGhostArr, ghostArr = 
    match arg with
      | `OfData (ttGA, gA) -> (Array.length gA, ttGA, gA)
      | `OfSize n -> (n, Array.make n None, Array.make n None)
  in

object (self)

  method getSize = size 

  method (* shallow *) copy = new ghostForest (
    `OfData (Array.copy taxonToGhostArr, Array.copy ghostArr))
                               
 (* init initializes things to be ready for coalescence *)
  method init = 
    for i=0 to size-1 do
      taxonToGhostArr.(i) <- Some i;
      ghostArr.(i) <- Some (Ghost.Leaf i);
    done

  method ghostIndexFromTaxon taxNum =
    match taxonToGhostArr.(taxNum) with
      | Some i -> i
      | None -> failwith ("Taxon "^(string_of_int taxNum)^" not found!")

  method ghostPresent i = 
    match ghostArr.(i) with
      | Some t -> true
      | None -> false

  method getGhostOpt i = ghostArr.(i)

  method getGhost i =
    match ghostArr.(i) with
      | Some t -> t
      | None -> failwith ("No ghost in location "^(string_of_int i)^"!")

  method ghostFromTaxon taxNum = 
    let i = self#ghostIndexFromTaxon taxNum in
    (* below: debugging. we should always have a ghost where taxonToGhostArr
    * says we do. *)
    assert(self#ghostPresent i); 
    self#getGhost i

  method setGhostOpt i tOpt = ghostArr.(i) <- tOpt
  method setGhost i t = self#setGhostOpt i (Some t)
  method setTaxonOptLoc i optLoc = taxonToGhostArr.(i) <- optLoc

  method getTaxonToGhostArr = taxonToGhostArr
  method getGhostArr = ghostArr
(* important for toSeparatingRsplitList that getGhostList is in increasing order
 * of index! *)
  method getGhostList = Base.extractSomes (Array.to_list ghostArr)

  method iter f = 
    Array.iter (
      function
        | Some ghost -> f ghost
        | None -> ()
    ) ghostArr

  method printGhosts = 
    Array.iter (
      function
        | Some ghost -> Printf.printf "%s\t" (Ghost.toString ghost)
        | None -> print_string "-\t"
    ) ghostArr;
    print_endline ""

  method nGhosts = 
    let count = ref 0 in
    self#iter (fun ghost -> incr count);
    !count

  method toTerminalSet = 
    let termSet = ref Rsplit.IntSet.empty in
    self#iter (
      fun ghost -> 
        termSet := Rsplit.IntSet.union !termSet (Ghost.toTerminalSet ghost)
    );
    !termSet

  method printTaxonToGhostArr = 
    print_endline (
      String.concat "; " (
        Array.to_list (
          Array.map (
            function 
              | Some i -> string_of_int i
              | None -> "-"
          ) taxonToGhostArr ))) 

  method print = 
    self#printGhosts;
    self#printTaxonToGhostArr

  method printShort = 
    self#printGhosts;

  method updateTaxonToGhostArr fromInd toInd = 
    for i = 0 to size-1 do
      if taxonToGhostArr.(i) = Some fromInd then
        taxonToGhostArr.(i) <- Some toInd
    done;

  (* coalesce: simply coalesce, no contract *)
  method coalesce ind1 ind2 = 
    assert (ind1 <> ind2);
    let sind1, sind2 = Base.sortPair (ind1, ind2) in
    ghostArr.(sind1) <- Some (Ghost.join (self#getGhost sind1) (self#getGhost sind2));
    ghostArr.(sind2) <- None;
   (* everything in the higher gets moved to the lower *)
    self#updateTaxonToGhostArr sind2 sind1

 (* coalesceLoose: only coalesce if legal, otherwise do nothing *)
  method coalesceLoose index1 index2 = 
    if self#ghostPresent index1 && self#ghostPresent index2 && index1 <> index2 then (
      self#coalesce index1 index2
    )

    (* isLegalCoal: is the given coalescence legal? *)
  method isLegalCoal ind1 ind2 = 
    ind1 <> ind2 && self#ghostPresent ind1 && self#ghostPresent ind2


  (* *** testing compatibility *** *)

(* convexWRTPartition: 
 * for a given partition and a given ghost, there are some partition states
 * which are "used"... those which have been seen but could not be the root
 * state, and the possible root states. we first need to check that the "used"
 * partition states are disjoint from any of the partition states used in the
 * other trees. but then we need to make sure that we can make a good assignment
 * of root states to the roots of the trees. if a given partition label shows up
 * in two different trees, then i call it an "enforced" root label... it must be
 * the root label. if there are two or more such enforced root labels, then we
 * have a nonconvex partition. note that pairwise checking of size is not
 * enough: consider ((0,1),(0,2),(1,2)), where the leaf labels are partition
 * states.
 * *)
  method convexWRTPartition partition = 
    (* if we have the trivial partition then yes *)
    if Rsplit.partitionSize partition = 1 then true
    else (
      try
        let checkDisjoint s1 s2 = 
          if not (IS.is_empty (IS.inter s1 s2)) then
            raise Ghost.PartitionIncompatible
        in
        let info = 
          Array.of_list (List.map (Ghost.getConvexInfo partition) self#getGhostList) in
        assert(Array.length info > 0);
        if Array.length info = 1 then 
          (* we don't have to worry about anything. if the tree is convex, then OK *)
          true
        else (
          let roots = Array.map fst info in
          let useds = Array.map snd info in
          let both = Array.map (fun (r,u) -> IS.union r u) info in
          let size = Array.length info in
          for i=0 to size-1 do
            let enforcedRootStates = ref IS.empty in
            for j=0 to size-1 do
              (* make sure that no useds intersect anything from the other trees *)
              if i <> j then (
                checkDisjoint useds.(i) both.(j);
                enforcedRootStates :=
                IS.union !enforcedRootStates (IS.inter roots.(i) roots.(j));
              )
            done;
            if 1 < IS.cardinal !enforcedRootStates then
              (* there are multiple enforced root states *)
              raise Ghost.PartitionIncompatible
          done;
          true
        )
      with
        | Ghost.PartitionIncompatible -> false
    )

  method toSeparatingRsplitList = 
    let ghostTaxSets = List.map Ghost.toTerminalSet (self#getGhostList) in
    assert(ghostTaxSets <> []);
    List.map Rsplit.unionOverList (
      List.filter (
        fun l -> List.length l > 1 
      ) (Base.list_powerSet (List.tl ghostTaxSets)))
  (* above: tl so that we don't get the zero tree *)

  method unfilteredToEdgeRsplitList = 
    Rsplit.rsplitList_uniques (
      let terminalSet = self#toTerminalSet in
      List.filter (fun rsplit -> not (IS.is_empty rsplit)) (
        List.flatten (
          List.map (
            function
              | Some ghost -> Ghost.toEdgeRsplitList terminalSet ghost
              | None -> []
          ) (Array.to_list ghostArr))))

  method toEdgeRsplitList = 
    List.filter (fun rsplit -> not (IS.is_empty rsplit)) (
      self#unfilteredToEdgeRsplitList)

(* the relevant rsplits are those that are needed for compatibility *)
  method toRelevantEdgeRsplitList = 
    List.filter (fun rsplit -> 1 < IS.cardinal rsplit) (
      self#unfilteredToEdgeRsplitList)

  method toAllRsplits = 
    Rsplit.rsplitList_uniques (
      (self#toEdgeRsplitList)@(self#toSeparatingRsplitList))

  method inducedGhostArr isIn = 
    Array.map (
      function 
        | Some ghost -> Ghost.inducedGhost isIn ghost
        | None -> None
    ) ghostArr

(* iterOverNontrivSeparatingPartitions:
 * iterate over the separating partitions with at least two ghosts in every
 * set, or the identity partition.
 * f should take an IntSetSet
 * we first make partitions of the ghost index set, then take the union across
 * all of the corresponding ghost taxon sets. 
 * *)
  method iterOverNontrivSeparatingPartitions f maxNParts =
    (* the sets of taxa in each ghost *)
    let taxonSets = Array.map ( 
      function
        | Some ghost -> Some (Ghost.toTerminalSet ghost)
        | None -> None
    ) ghostArr in
    (* occupieds: which ghost indices have taxa in them? *)
    let occupieds = ref [] in
    Array.iteri (
      fun index -> 
        function 
          | Some g -> occupieds := !occupieds @ [index]
          | None -> ()
    ) ghostArr;
    assert(!occupieds <> []);
    (* if there is only a single ghost in the forest *)
    let singleGhost = (List.length !occupieds = 1) in
    Rsplit.iterOverIntSetPartitions (
      fun ghostIndexPart ->
        if singleGhost || Rsplit.setSizeAtLeast 2 ghostIndexPart then
          f (
            Rsplit.IntSetSet.fold (
              fun ghostIndSet intSetSetSoFar ->
                (* add to the partition on taxon labels *)
                Rsplit.IntSetSet.add (
                  (* the union of the taxon sets of the things in the ghostIndSet *) 
                  Rsplit.IntSet.fold (
                    fun ghostIndex taxonSetSoFar ->
                      match taxonSets.(ghostIndex) with
                        | Some ghostSet -> 
                            Rsplit.IntSet.union taxonSetSoFar ghostSet
                        | None -> 
                            failwith "iterOverNontrivSeparatingPartitions: no set!"
                  ) ghostIndSet Rsplit.IntSet.empty 
                ) intSetSetSoFar
            ) ghostIndexPart Rsplit.IntSetSet.empty 
          )
    ) !occupieds maxNParts

(* findGoodMPartition
 start with the separating partition, can go from size 1 to m, then iter through
 the sets of edge splits 
 iter through the separating partitions, then iter through the edge sets
 *)

  method findGoodMPartition isGoodFn m = 
    try
      let edgeRsplits = self#toEdgeRsplitList in
      self#iterOverNontrivSeparatingPartitions (
        fun sepPart ->
        (* the number of partitions we get just from separation *)
          let nPartCuts = Rsplit.IntSetSet.cardinal sepPart - 1 in
          Rsplit.iterOverCompatPartitions (
            fun partIntSetSet ->
              assert(m+1 = Rsplit.IntSetSet.cardinal partIntSetSet);
              let partition = Rsplit.partIntArrOfIntSetSet size partIntSetSet in
              if isGoodFn partition then raise (GoodPartition partition)
          (* below: we want m+1 sets in total. there are (nPartSet-1) cuts already
           * with the separating partition. *)
          ) (m - nPartCuts) sepPart edgeRsplits
      ) (m+1); (* here m+1 is the max number of sets for the sep partition *)
      None
    with
      | GoodPartition partition -> Some partition
      | Failure s -> (
          self#print;
          Printf.printf "%d\n" m;
          failwith s
            
        )


                       (*
  method iterOverPartitions f m = 
    let edgeRsplits = self#toEdgeRsplitList in
    self#iterOverNontrivSeparatingPartitions (
      fun sepPart ->
        print_endline "hi";
        let nPartCuts = Rsplit.IntSetSet.cardinal sepPart - 1 in
        Rsplit.iterOverCompatPartitions f
          (m - nPartCuts) sepPart edgeRsplits
    ) (m+1) (* here m is the max number of sets for the sep partition *)
                        *)
      

end   (* of object *)
