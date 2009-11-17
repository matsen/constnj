
class njForest arg = 
  let forest, mu, q, tau = 
    match arg with
    | `OfForestAndMuTau (givenForest, givenMu, givenTau) -> 
        givenForest, givenMu, Quemu.qOfMu givenMu, givenTau
    | `OfDistMat distMat -> 
        let mu = Quemu.muOfDistMat distMat in
        let f = new Forest.forest (`OfSize (SymmMat.size distMat)) in
        f#init;
        f, mu, Quemu.qOfMu mu, Quemu.upperTriTotal mu
  in

  object(self)

    inherit Forest.forest (`OfTreeArr forest#getTreeArr) as super
    method getForest = forest
    method getMu i j = mu.(i).(j)
    method getMuMat = mu
    method getQ i j = q.(i).(j)
    method getQMat = q
    method getTau = tau

    method printForest = super#print
    method print =
      super#print;
      (* print_endline "mu:";*)
      (* SymmMat.print mu;*)
      (* print_endline "";*)
      print_endline "Q:";
      SymmMat.print q;
      print_endline "";
      Printf.printf "Tau: %g\n\n" tau;
      ()

    (* mat means matrix indices *)
    method matCoalesceToNew matrixInd1 matrixInd2 = 
      let newForest = forest#copy in
      newForest#coalesce 
        (self#matrixToForestIndex matrixInd1)
        (self#matrixToForestIndex matrixInd2);
      new njForest (
        `OfForestAndMuTau 
          (newForest, 
           Quemu.coalMu mu matrixInd1 matrixInd2, 
           tau -. q.(matrixInd1).(matrixInd2)))

    method bestCoal = 
      assert(Array.length q >= 2);
      List.fold_left (
        fun (a1, a2) (b1, b2) -> 
          if q.(a1).(a2) > q.(b1).(b2) then (a1, a2) else (b1, b2)
      ) (0,1) (Base.getUpperTriIndices (Array.length q))

    method njCoalToNew = 
      let mind1, mind2 = self#bestCoal in
      self#matCoalesceToNew mind1 mind2

    method toObtree = 
      let trees = super#getTreeList in
      assert(0 < List.length trees && List.length trees <= 3);
      List.fold_left Obtree.join (List.hd trees) (List.tl trees)

  end

let rec runNJ h = 
  if h#nTrees <= 3 then h
  else runNJ h#njCoalToNew

let runNJOnDM distMat = 
  let out = runNJ (new njForest (`OfDistMat distMat)) in
  (out#getTau, Obtree.reroot out#toObtree)


(* **** testing **** *)

let rec doFinalCoals h = 
  if h#nTrees > 3 then (
    h#print;
    failwith "wow: didn't get to three trees!"
  );
  if h#nTrees = 1 then h
  else doFinalCoals (h#matCoalesceToNew 0 1)

let postProc completedH = 
  let t = completedH#getTree 0 in
  Obtree.reroot t

let njTest n = 
  let orig = Betree.unifYule n in
  Printf.printf "tree length: %g\n" (Betree.notRootTreeLength orig);
  print_endline "dist mat:";
  Mat.AAR.print (Betree.toDistMat orig);
  print_endline "";
  let h = runNJ (new njForest (`OfDistMat (Betree.toDistMat orig))) in
  h#printForest;
  Printf.printf "tau: %g\n" h#getTau;
  let outTree = postProc (doFinalCoals h) in
  let origOB = Obtree.reroot (Betree.toObtree orig) in
  if outTree = origOB then (
    print_endline "same same";
    print_endline ("final:    "^(Obtree.toString outTree));
  )
  else (
    print_endline "trees differ: ";
    print_endline ("original: "^(Obtree.toString origOB));
    print_endline ("final:    "^(Obtree.toString outTree));
  )

let bigTest maxSize nReps = 
  for i=0 to nReps do
    njTest (4+(Random.int (maxSize-4)))
  done;
  ()

    (* note just compares trees *)
let equal njf1 njf2 = 
  Forest.equal njf1#getForest njf2#getForest
