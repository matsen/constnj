
let rng = Base.makeRng 0

let betCalcMVal bet1 bet2 = 
  let (gf1, gf2) = GhostForestFuns.gfOfObtreePair 
                     (Betree.toObtree bet1) 
                     (Betree.toObtree bet2) in
  let partition = MAP.find gf1 gf2 in
  Rsplit.partitionMVal partition

let rSprPair rng expParamTotal nTaxa nSprs = 
  let ourExp () = Gsl_randist.exponential rng (expParamTotal /. (float_of_int (2*(nTaxa-1)))) in 
  let treeList = Betree.yuleRsprList ourExp nTaxa 2 nSprs in
  (List.nth treeList 0, List.nth treeList 1)

let trueRSprPair rng expParamTotal nTaxa nSprs = 
  let rec aux () =
    let bet1, bet2 = rSprPair rng expParamTotal nTaxa nSprs in
    if betCalcMVal bet1 bet2 = nSprs then (bet1, bet2) else aux ()
  in
  aux ()

let getSimdLengths rng nTaxa nTrees nSprs expParamTotal seqLength maxSprs = 
   let bet1, bet2 = trueRSprPair rng expParamTotal nTaxa nSprs in
   let dms = Array.map (Sim.toGoodJCPerturbedMat rng seqLength) [|bet1; bet2|] in
   let results = 
     Step.run 0
       (new Instance.instance 
          (`OfDMsAndForestIndPairLimits (dms, [|((0, 1), maxSprs)|]))) in
   Array.map ( 
     function
       | Some inst -> Some inst#getLength
       | None -> None
   ) results#getTensorArr

(*
# extendLastSome [| Some 1.; Some 0.5; None; Some 0.25; None |];;
- : float array = [|1.; 0.5; 0.5; 0.25; 0.25|]
 *)
let extendLastSome a = 
  assert(a <> [||]);
  let last = ref a.(0) in
  let len = Array.length a in
  let extended = Array.make len 0. in
  for i=0 to len-1 do
    extended.(i) <- (
      match a.(i) with
        | Some currLen -> last := a.(i); currLen
        | None -> (
            match !last with
              | Some lastLen -> lastLen
              | None -> assert(false)
          ) )
  done;
  extended


let collectResults runArr =
  assert(runArr <> [||]);
  let len = Array.length runArr.(0) in
  let results = ref (Array.make len 0.) in
  Array.iter (
    fun newResult ->
      results := Base.array_map2 ( +. ) !results newResult
  ) runArr;
  Array.map (fun x -> x /. (float_of_int (Array.length runArr))) !results
  
let avgSimdLengths rng nRuns nTaxa nTrees nSprs expParamTotal seqLength maxSprs = 
  collectResults (
    Array.init nRuns (
      fun i -> 
        if nRuns > 10 && i mod (nRuns/10) = 0 then Printf.printf "doing %d of %d...\n" i nRuns;
        flush_all ();
        extendLastSome (
          getSimdLengths rng nTaxa nTrees nSprs expParamTotal seqLength maxSprs)))

    (*
let rng = Base.makeRng 0
let x = avgSimdLengths rng 2 10 2 2 0.5 1000 4
     *)

let runLt ctlFname = 
  let (ih, fh, bh, sh, existence_fun) = Ctl.ofFiles [ctlFname] in
  let rng = Base.makeRng (ih "seed") in
  let out = open_out ((Base.basename ctlFname)^".out") in

  List.iter (
    fun s ->
      Printf.fprintf out "# %s\n" s
  ) (Base.stringListOfFile ctlFname);

  let lengths = 
    avgSimdLengths rng 
      (ih "nRuns")
      (ih "nTaxa")
      2
      (ih "nSprs")
      (fh "expParamTotal")
      (ih "seqLength")
      (ih "maxSprs")
  in

  let printArr = Array.iter (fun x -> Printf.fprintf out "%g\n" x) in
  printArr lengths;
  (* Printf.fprintf out "\n\n";*)

  close_out out;
  ()


let () = (
  if not !Sys.interactive then (

    Arg.parse [] runLt ""

  )
)
