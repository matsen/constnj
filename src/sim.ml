exception Bad_correction
let maxNBadMatrices = 100

let jcCorr subsFract =  
  let x = -. 0.75 *. (log ( 1. -. (4./.3.) *. subsFract )) in
  if Base.isOKFloat x then x 
  else raise Bad_correction

let nObservableJCMuts rng branchLength seqLength = 
  let nMuts = ref 0 in
  for i=1 to seqLength do
    (* below: Gsl wants mean mu, not lambda *)
    if Gsl_randist.exponential rng (3. /. (4. *. branchLength)) < 1. &&  (* we have an event *)
       1 = Gsl_randist.bernoulli rng 0.75 then (* this event results in an observable mut *)
      nMuts := !nMuts + 1
  done;
  !nMuts

let observableJCMutsPerSite rng branchLength seqLength = 
  assert(seqLength <> 0);
  Base.int_div (nObservableJCMuts rng branchLength seqLength) seqLength

(* let perturbBL rng branchLength seqLength = jcCorr (observableJCMutsPerSite
 * rng branchLength seqLength) *)

let toJCNMutMat rng seqLength beTree = 
  Betree.toDistMat (
    Betree.mapEdgeLengths (
      fun branchLength ->
        observableJCMutsPerSite rng branchLength seqLength
    ) beTree )

(* only return DMs without nans *)
let toGoodJCPerturbedMat rng seqLength beTree = 
  let count = ref 0 in
  let rec aux () = 
    try 
      Mat.AAR.map jcCorr (toJCNMutMat rng seqLength beTree)
    with | Exit -> (
      count := !count + 1;
      if !count <= maxNBadMatrices then aux ()
      else failwith "toGoodJCPerturbedMat: exceeded limit of bad matrices" 
    )
  in
  aux ()
