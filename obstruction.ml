class obstruction arg = 

  let checkPartition gf0 gf1 partition = 
    if not (MAP.partitionIsAP partition gf0 gf1) then (
      gf0#print; 
      gf1#print; 
      Base.printIntArr partition;
      let _ = MAP.verbosePartitionIsAP partition gf0 gf1 in
      invalid_arg "mapInstance constructor: partition is not an agreement partition"
    )
  in

  let gf0, gf1, oPartn = 
    match arg with
    | `OfData (givenGF0, givenGF1, givenOPart) -> 
        checkPartition givenGF0 givenGF1 givenOPart;
        givenGF0, givenGF1, givenOPart
    | `OfSize n -> 
        let gf1 = new GhostForest.ghostForest (`OfSize n)
        and gf2 = new GhostForest.ghostForest (`OfSize n) in
        gf1#init;
        gf2#init;
        gf1, gf2, Array.make n 0
  in

object(self)

  method copy = 
    new obstruction (`OfData (gf0#copy, gf1#copy, Array.copy oPartn))
  method getGF which = if which = false then gf0 else gf1
  method getSize = assert(gf0#getSize = gf0#getSize); gf0#getSize
  method getPartition = oPartn
  method getMAPMVal = Rsplit.partitionMVal oPartn
  method getNGhosts = gf0#nGhosts, gf1#nGhosts

  method print = gf0#print; gf1#print; Base.printIntArr oPartn

  method partitionIsAP partition = 
    MAP.partitionIsAP partition gf0 gf1

 (* perhapsCoalesceContractToNew: return an opt obstruction, which is Some obs
  * if findWithinLimits finds us a MAP within the given limits. 
  * *)
  method perhapsCoalesceContractToNew which index1 index2 optMUpper =
    let newGF0 = gf0#copy
    and newGF1 = gf1#copy in
    if which = false then 
      GhostForestFuns.coalesceContract newGF0 newGF1 index1 index2
    else
      GhostForestFuns.coalesceContract newGF1 newGF0 index1 index2;
    if MAP.partitionIsAP oPartn newGF0 newGF1 then 
      Some (new obstruction (`OfData (newGF0, newGF1, oPartn)))
    else 
      match MAP.findWithinLimits newGF0 newGF1 (Rsplit.partitionMVal oPartn) optMUpper with
        | Some newPartn -> 
            if Rsplit.partitionMVal newPartn < Rsplit.partitionMVal oPartn then
              failwith "MAP size decreased!";
            Some (new obstruction (`OfData (newGF0, newGF1, newPartn)))
        | None -> None

  method coalesceContractToNew which index1 index2 =
    match self#perhapsCoalesceContractToNew which index1 index2 None with
      | Some newObs -> newObs
      | None -> assert(false)

  method isLegalCoal which index1 index2 = 
    (self#getGF which)#isLegalCoal index1 index2

end


(* for casual use only. may or may not return a new obstruction *)
let looseCoalesceContract obs which index1 index2 = 
  if obs#isLegalCoal which index1 index2 then 
    obs#coalesceContractToNew which index1 index2
  else obs

let yule n nCoals = 
  let rec aux obs coalsLeft = 
    let (ng0, ng1) = obs#getNGhosts in
    if coalsLeft > 0 && (ng0 > 1 || ng1 > 1) then 
      aux 
        (looseCoalesceContract 
           obs 
           (Random.bool ())
           (Random.int n)
           (Random.int n))
        (coalsLeft-1)
    else 
      obs
  in
  aux (new obstruction (`OfSize n)) nCoals
