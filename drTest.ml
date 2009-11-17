let totalRF tArr1 tArr2 = 
  Array.fold_left ( + ) 0 (
    Base.array_map2 (
      fun t1 t2 -> Obtree.rfDistance t1 t2) tArr1 tArr2)

let runTest rng verbosity nTaxa nTrees nSprs expParamTotal seqLength = 
  let ourExp () = Gsl_randist.exponential rng (expParamTotal /. (float_of_int (2*(nTaxa-1)))) in 
  let rec aux () = 
    try (
      let trees = 
        Array.of_list (
          Betree.yuleRsprList ourExp nTaxa nTrees nSprs) in
      (* let dms = Array.map Betree.toDistMat trees in*)
      let mutMats = Array.map (Sim.toJCNMutMat rng seqLength) trees in
      let (_, concatTree) = 
        NjForest.runNJOnDM (
          (Mat.AAR.map Sim.jcCorr) (
            Array.fold_left 
              Mat.AAR.add 
              (Mat.AAR.zero nTaxa nTaxa) 
              (Array.map 
                 (* have to divide by number of matrices so that we get number of muts per site *)
                 (Mat.AAR.map (fun x -> x /. (float_of_int (Array.length mutMats)))) 
                 mutMats))) in
      let dms = Array.map (Mat.AAR.map Sim.jcCorr) mutMats in
      let constraints = Constraints.linear nTrees nSprs in
      let results = Step.run verbosity (
        new Instance.instance 
          (`OfDMsAndForestIndPairLimits (dms, constraints))) in
      let inTrees = Array.map Betree.toObtree trees in
      match
        Base.array_min (
          Base.optCompare (
            fun i1 i2 -> compare i1#getLength i2#getLength))
          results#getTensorArr 
      with 
        | Some shortestInst ->
            let indepTrees = 
          Array.map snd (Array.map NjForest.runNJOnDM dms) in
            (* postProc-like behavior *)
            (totalRF inTrees (Array.map Obtree.reroot shortestInst#toObtreeArr),
             totalRF inTrees (Array.make nTrees concatTree),
             totalRF inTrees indepTrees)
        | None -> failwith "No trees reconstructed!"
    )
    with
      | Sim.Bad_correction -> aux ()
  in
  aux ()

let runAvgTest rng nTests nTaxa nTrees nSprs expParamTotal seqLength = 
  let distrecScore = ref 0
  and concatScore = ref 0
  and indepScore = ref 0 in
  for test=1 to nTests do
    let (d, c, i) = runTest rng 0 nTaxa nTrees nSprs expParamTotal seqLength in
    distrecScore := !distrecScore + d;
    concatScore := !concatScore + c;
    indepScore := !indepScore + i;
    if test mod (nTests/10) = 0 then (Printf.printf "%d\n" test; flush_all ());
  done;
  assert (nTests <> 0);
  (Base.int_div !distrecScore nTests,
   Base.int_div !concatScore nTests,
   Base.int_div !indepScore nTests)

let () = 
 if not !Sys.interactive then (
   let (ih, fh, bh, sh, existence_fun) = Ctl.ofFiles (Ctl.find_all_ctls ["../.."; ".."; "."]) in

   let rng = Base.makeRng (ih "seed") in
   let (avgDistrecScore, avgConcatScore, avgIndepScore) =
     runAvgTest rng 
       (ih "nTests")
       (ih "nTaxa")
       (ih "nTrees")
       (ih "nSprs")
       (fh "expParamTotal")
       (ih "seqLength") in
   let ch = open_out "drt.out" in
   Printf.fprintf ch "%d\t%g\t%g\t%g\t%g\n" 
     (ih "seqLength")
     (fh "expParamTotal")
     avgDistrecScore
     avgConcatScore
     avgIndepScore;
   close_out ch;

   Printf.printf "elapsed time: %s\n" (Base.timeStrOfSeconds (Sys.time ()));
 )

  (*
let rng = Base.makeRng 0
let x = runAvgTest rng 30 3 10 3 1 0.01 10000
   *)
