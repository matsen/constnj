(* in this test we show that distrec "interpolates" between the two extremes of
 * concatenated and independent reconstruction *)

let totalRF tArr1 tArr2 = 
  Array.fold_left ( + ) 0 (
    Base.array_map2 (
      fun t1 t2 -> Obtree.rfDistance t1 t2) tArr1 tArr2)

let runTest rng verbosity nTaxa nGenSprs nAllowedSprs expParamTotal seqLength = 
  let ourExp () = Gsl_randist.exponential rng (expParamTotal /. (float_of_int (2*(nTaxa-1)))) in 
  let rec aux () = 
    try (
      let trees = 
        Array.of_list (
          Betree.yuleRsprList ourExp nTaxa 2 nGenSprs) in
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
      let constraints = Constraints.linear 2 nAllowedSprs in
      let results = Step.run verbosity (
        new Instance.instance 
          (`OfDMsAndForestIndPairLimits (dms, constraints))) in
      let inTrees = Array.map Betree.toObtree trees in
      let indepTrees = Array.map snd (Array.map NjForest.runNJOnDM dms) in
      ( Array.map ( 
        function
          | Some trees ->
              Some (totalRF inTrees (Array.map Obtree.reroot trees#toObtreeArr))
          | None -> None
      ) (results#getTensorArr),
        totalRF inTrees (Array.make 2 concatTree),
        totalRF inTrees indepTrees)
    )
    with
      | Sim.Bad_correction -> aux ()
  in
  aux ()




let runAvgTest rng nTests nTaxa nGenSprs nAllowedSprs expParamTotal seqLength = 
  let dArrList = ref [] 
  and concatScore = ref 0
  and indepScore = ref 0 in
  for test=1 to nTests do
    let (dArr, c, i) = runTest rng 0 nTaxa nGenSprs nAllowedSprs expParamTotal seqLength in
    dArrList := dArr::(!dArrList);
    concatScore := !concatScore + c;
    indepScore := !indepScore + i;
    if nTests >= 10 && test mod (nTests/10) = 0 then (Printf.printf "%d\n" test; flush_all ());
  done;
  assert (nTests <> 0);
  let x = 
    Array.map (
      fun l -> Base.list_avgFloats (List.map float_of_int l)) (
        List.fold_left (
          fun soFar testResult ->
            Base.array_map2 (
              fun resultList ->
                function 
                  | Some x -> x::resultList
                  | None -> resultList
            ) soFar testResult) (Array.make (nAllowedSprs+1) []) !dArrList) in
  (x,
   Base.int_div !concatScore nTests,
   Base.int_div !indepScore nTests)

     
let () = 
 if not !Sys.interactive then (
   let (ih, fh, bh, sh, existence_fun) = Ctl.ofFiles (Ctl.find_all_ctls [".."; "."]) in

   let rng = Base.makeRng (ih "seed") in
   let (avgDistrecScoreArr, avgConcatScore, avgIndepScore) =
     runAvgTest rng 
       (ih "nTests")
       (ih "nTaxa")
       (ih "nGenSprs")
       (ih "nAllowedSprs")
       (fh "expParamTotal")
       (ih "seqLength") in
   let ch = open_out "interp.out" in
   Printf.fprintf ch "%d\t%g\t%g\t" 
     (ih "seqLength")
     avgConcatScore
     avgIndepScore;
   Array.iter (fun x -> Printf.fprintf ch "%g\t" x) avgDistrecScoreArr;
   Printf.fprintf ch "\n";
   close_out ch;

   Printf.printf "elapsed time: %s\n" (Base.timeStrOfSeconds (Sys.time ()));
 )


(*
let rng = Base.makeRng 0
let x = runAvgTest rng 10 3 10 2 5 0.5 500
 *)
  

