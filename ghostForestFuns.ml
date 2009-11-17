
let gfsEqual gf1 gf2 = 
  (gf1#getTaxonToGhostArr = gf2#getTaxonToGhostArr) &&
  (gf1#getGhostArr = gf2#getGhostArr)

(* coalesceContract: coalesce trees ind1 and ind2 in ghost forest toCoalGF,
 * while seeing if we can contract with respect to forest otherGF. *)
let coalesceContract toCoalGF otherGF ind1 ind2 = 
  let sind1, sind2 = Base.sortPair (ind1, ind2) in
  let t = toCoalGF#getGhost sind1 
  and s = toCoalGF#getGhost sind2 in
  toCoalGF#coalesce sind1 sind2;
  (* coalescedGhost is what we have after coalescence *)
  let coalescedGhost = toCoalGF#getGhost sind1 in
  (* now think about contracting *)
  if otherGF#ghostIndexFromTaxon sind1 = otherGF#ghostIndexFromTaxon sind2 then (
    (* the two taxa are in the same ghost... they might form a contractible unit *)
    (* potentiallyContractibleLocation *)
    let potContrLoc = otherGF#ghostIndexFromTaxon sind1 in
    let potContrGhost = otherGF#getGhost potContrLoc in
    if Ghost.isTerminal t && Ghost.isTerminal s then (
      (* we are coalescing to make a cherry *)

      (* the index of the ghosts containing the Leaf/CTs should be the same as
       * their index. perhaps cut this later *)
      assert ( Ghost.getIndex t = sind1 );
      assert ( Ghost.getIndex s = sind2 );

      match Ghost.contractCherry potContrGhost sind1 sind2 with
        | true, newGGhost ->
            (* we have contracted a cherry *)
            toCoalGF#setGhost sind1 ( Ghost.CT ( sind1 ) ) ;
            otherGF#setGhost potContrLoc newGGhost ;
        | false, sameGhost -> ()
    )
    else (
      (* ok, we didn't just make a cherry, what about triples or extendos? *)
      let topTrips = Ghost.topTriples coalescedGhost in
      if topTrips <> [] then (
        (* we have some triples (at most two), maybe we can contract them *)
        let firstTrip = List.hd topTrips in
        let contRoot = Ghost.containsRoot coalescedGhost in
        (* we don't want to contract a chain on one side that has the root and
         * on the other side doesn't contain the root. then the order would be
         * wrong. e.g. (1,(2,(3,0))) and (1,(2,(3,4))) should not be turned into
         * [0|1] and [4|1]... the order's wrong. so we first determine if
         * coalescedGhost has the root, then only contract if potContrGhost also
         * has the root *)
        match Ghost.contractChain potContrGhost firstTrip contRoot with
          | true, newGGhost ->
              (* we have contracted a chain in otherGF *)
              toCoalGF#setGhost sind1 ( 
                Ghost.certainContractChain coalescedGhost firstTrip contRoot);
              otherGF#setGhost potContrLoc newGGhost;
          | false, sameGhost -> (
              if List.tl topTrips <> [] then (
                (* try again with the second triple *)
                let secTrip = List.hd ( List.tl topTrips ) in
                match Ghost.contractChain potContrGhost secTrip contRoot with
                  | true, newGGhost ->
                      (* we have contracted a chain *)
                      toCoalGF#setGhost sind1 ( 
                        Ghost.certainContractChain coalescedGhost secTrip contRoot);
                      otherGF#setGhost potContrLoc newGGhost;
                  | false, sameGhost -> ()
              )
            )
      )
      else (
        (* perhaps there is an extendo *)
        match Ghost.findTopExtendo coalescedGhost with
          | Some (ctOrLeaf, cc) -> (
              match Ghost.contractExtendo 
                      potContrGhost 
                      ( Ghost.getCCLabel cc ) 
                      ( Ghost.getIndex ctOrLeaf ) 
              with
                | true, newGGhost ->
                    (* we have contracted an extendo *)
                    toCoalGF#setGhost sind1 ( 
                      Ghost.certainContractExtendo 
                        coalescedGhost
                        ( Ghost.getCCLabel cc ) 
                        ( Ghost.getIndex ctOrLeaf ) 
                        );
                    otherGF#setGhost potContrLoc newGGhost;
                | false, sameGhost -> ()
            )
          | None -> ()
      )
    )
  )


(* getCCs:
 * return an array with all of the CCs, indexed by their uppermost label. note
 * that if there are nested CC's then the lower ones of the nest will be
 * repeated. as usual, we assume that all indices are distinct. *)
let getCCs gf = 
  let ccs = Array.make gf#getSize None in
  let rec addCCs = function
    | Ghost.Node(a, b, i) -> addCCs a; addCCs b
    | Ghost.CC(a, i) -> 
        ccs.(i) <- Some (Ghost.CC(a, i)); addCCs a
    | Ghost.CT(i) -> () 
    | Ghost.Leaf(i) -> () 
  in
  gf#iter addCCs;
  ccs

(* getCCComponents: 
 * get the induced subghosts which are in the same partition as the various
 * CC's. we need for these to be equal between the two ghost forests in order
 * go have an agreement partition. because CC's come with a natural rooting,
 * it's not enough to just check the splits. *)
let getCCComponents partition gf = 
  try
    Array.map (
      function
        | Some cc ->
            Some (Ghost.inducedGhost (
              fun index ->
                partition.(index) = partition.(Ghost.getCCLabel cc)
            ) cc)
        | None -> None
    ) (getCCs gf)
  with
    | Invalid_argument s -> failwith ("getCCComponents: partition too small "^s)

let prettyPrint ff gf = 
  Format.open_box 0;
  gf#iter (
    fun ghost ->
      Format.fprintf ff "%s" ghost#toString;
      Format.print_space ();
  );
  Format.close_box ()

let ttGAOfGhostArr ghostArr = 
  let ttGA = Array.make (Array.length ghostArr) None in
  Array.iteri (
    fun i -> function
      | Some ghost ->
          List.iter (
            fun tax -> 
              if ttGA.(tax) <> None then
                failwith "taxon repeated in ttGAOfGhostArr!";
              ttGA.(tax) <- Some i
          ) (Ghost.getTerminalNumbers ghost)
      | None -> ()
  ) ghostArr;
  ttGA

(* doesn't make complete checks *)
let gfOfGhostArr ghostArr = 
  new GhostForest.ghostForest (
    `OfData(ttGAOfGhostArr ghostArr, ghostArr))

let gfOfStringArr strArr = 
  gfOfGhostArr (
    Array.map (
      fun str ->
        if str = "" then None
        else Some (Ghost.ofNewick str)
    ) strArr)

let gfOfObtreePair obt1 obt2 = 
  let n = Obtree.nLeaves obt1 in
  assert(n = Obtree.nLeaves obt2);
  Obtree.checkTaxa obt1;
  Obtree.checkTaxa obt2;
  let gf1 = new GhostForest.ghostForest (`OfSize n)
  and gf2 = new GhostForest.ghostForest (`OfSize n) in
  gf1#init;
  gf2#init;
  List.iter (fun (i,j) -> coalesceContract gf1 gf2 i j) 
            (Obtree.toCoalSeq obt1);
  List.iter (fun (i,j) -> coalesceContract gf2 gf1 i j) 
            (Obtree.toCoalSeq obt2);
  (* gf1#print; gf2#print; *)
  (gf1, gf2)

(* *** testing *** *)

let partialYule n nSteps = 
  assert(n-nSteps >= 1);
  let f = new GhostForest.ghostForest (`OfSize n) in
  f#init;
  while f#nGhosts > n-nSteps do
    f#coalesceLoose (Random.int n) (Random.int n)
  done;
  f

(* newPartial: just do a certain number of coals *)
let newPartial n maxSteps coalList = 
  let f = new GhostForest.ghostForest (`OfSize n) in
  f#init;
  let count = ref 0 in
  try
    List.iter (
      fun (ind1, ind2) ->
        if !count >= maxSteps then raise Exit;
        incr count;
        f#coalesce ind1 ind2
    ) coalList;
    f
  with
    | Exit -> f

let newComplete n coalList = 
  newPartial n (List.length coalList) coalList

let ofNewick s = 
  let g = Ghost.ofNewick s in
  let tn = List.sort compare (Ghost.getTerminalNumbers g) in
  let n = List.length tn in
  if tn <> Base.toNList (n-1) then failwith (s^"tree does not have taxa 0,...,n");
  newComplete n (Ghost.toCoalSeq (Ghost.ofNewick s))

(* the following is a silly hack. bad ttGA! *)
let ofGhostList length gList = 
  let arr = Array.make length None in
  let ttGA = Array.make length None in
  List.iter (
    fun g -> 
      arr.(Ghost.getIndex g) <- Some g
  ) gList;
  new GhostForest.ghostForest (`OfData (ttGA, arr))


(*
let gf = ofGhostList 11 [Ghost.g; Ghost.gp;]
let f = partialYule 6 5;;
f#print;;
let x = f#iterOverNontrivSeparatingPartitions Rsplit.printIntSetSet 1
let x = f#iterOverPartitions Rsplit.printIntSetSet 2
 *)
