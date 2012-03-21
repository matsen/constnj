(* step.ml
 * a "step" is a tensor of instance options. we get steps by coalescing a given
 * instance in various ways.
 * *)

let getAllCoals step = 
  let coals = ref [] in
  step#iteri (
    fun mInd -> function
      | Some inst -> 
          coals := (
            List.map (
              fun (forestInd, ind1, ind2) ->
                (mInd, forestInd, ind1, ind2)
            ) (inst#getCoals))::!coals;
      | None -> ()
  );
  List.flatten !coals

let getInst step mInd =
  match step#get mInd with
    | Some inst -> inst
    | None -> invalid_arg "empty step in getInst"

let getNewLength step mInd forestInd ind1 ind2 = 
  let inst = getInst step mInd in
  inst#getLength -. inst#getQ forestInd ind1 ind2

let sortedCoals step = 
  List.sort (
    fun (aMInd, aForestInd, aInd1, aInd2) (bMInd, bForestInd, bInd1, bInd2) ->
            compare (getNewLength step aMInd aForestInd aInd1 aInd2)
                    (getNewLength step bMInd bForestInd bInd1 bInd2))
    (getAllCoals step)

let getOccupieds step = 
  let occupied = ref [] in
  step#iteri (
    fun mInd -> function
      | Some inst -> occupied := mInd::(!occupied)
      | None -> ()
  );
  List.rev !occupied

(* fastStepStep sorts all of the coalescences from all present individuals
 * first, and performs coalescences in that order. 
 * also requires that new steps are strictly better than previous ones using
 * the partial order on mInds. *)
let fastStepStep verbosity step = 
  let identical = Array.make step#getNDims 0 in (* the profile of identical trees *)
  let newStep = new Tensor.tensor (`Make(step#getDimArr, None)) in
  let getFromStep mInd = 
    match step#get mInd with | Some inst -> inst | None -> assert(false)
  in
  let rec aux = function
    | (mInd, forestInd, ind1, ind2)::rest -> (
        let occMInds = getOccupieds newStep in
        let improvements = MIndPoset.improvementMInds occMInds in
  (* below: if the incoming mInd isn't already an improvement then skip. *)
        if MIndPoset.notGreaterThanList mInd improvements then (
          let stepResults = 
            if occMInds <> [] then 
                (* we are required to do better than previous steps *)
                (getFromStep mInd)#constrainedCoalesceToNew 
                  forestInd ind1 ind2 (MIndPoset.mustDoBetterThan occMInds)
            else
              (* nothing occupied yet. just enforce the constraints *)
              (getFromStep mInd)#perhapsCoalesceToNew forestInd ind1 ind2 
          in
          match stepResults with
            | Some newCoal -> 
                (* this coalescence satisfies the constraints *)
                let newProfile = newCoal#getProfile in
                assert(step#isLegalMIndex newProfile);
                if newStep#get newProfile = None then (
                  (* this is the first time we've seen this profile *)
                  newStep#set newProfile (Some newCoal);
                  if verbosity >= 1 then (
                    Printf.printf "   %s" ((Base.intArrToString newProfile)^" ");
                    if verbosity >= 2 then
                      Printf.printf ": %g\n" newCoal#getLength;
                    flush stdout;
                  );
                );
                (* next, recur if the "identical" spot isn't filled yet *)
                if newStep#get identical = None then aux rest
            | None -> (* we haven't filled anything, so keep recurring *)
                aux rest
        )
        else aux rest
      )
    | [] -> ()
  in 
  aux (sortedCoals step);
  if verbosity = 1 then print_endline "";
  newStep

let combineSteps how step1 step2 = 
  Tensor.map2 (
    fun arg1 arg2 ->
      match (arg1, arg2) with
        | (Some i1, Some i2) -> Some (how i1 i2)
        | (Some i, None) -> Some i
        | (None, Some i) -> Some i
        | (None, None) -> None
  ) step1 step2

    (* shortest wins *)
let combineStepsShortest s1 s2 = 
  combineSteps 
    (fun i1 i2 -> if i1#getLength < i2#getLength then i1 else i2) s1 s2

(* stepStep actually makes every best step from every current individual, then
 * finds the best one. Not used in current constNJ. *)
let stepStep step = 
  let currStep = ref (new Tensor.tensor (`Make(step#getDimArr, None))) in
  step#iter (
    function
      | Some inst -> 
          currStep := combineStepsShortest !currStep (inst#makeStep)
      | None -> ()
  );
  !currStep

let onlyNone step = 
  let ifNone = ref true in
  step#iter (
    function 
      | Some inst -> ifNone := false
      | None -> ()
  );
  !ifNone

let nCoalsLeft step = 
  let value = ref None in
  step#iter (
    function
      | Some inst -> (
          let newVal = inst#getNCoalsLeft in
          match !value with 
      (* make sure all entries of the step have the same number of coals left *)
            | Some n -> assert(n = newVal);
            | None -> value := Some newVal
        )
      | None -> ()
  );
  !value
            
let printStep step = 
  step#iteri (
    fun mInd x -> 
      match x with
        (* | Some inst -> inst#print*)
        | Some inst -> 
            print_endline "";
            print_endline (">>>> tensor at "^(Base.intArrToString mInd)^": ");
            inst#printObsArr
        | None -> ()
  )

let completePrint step = 
  step#iteri (
    fun mInd x -> 
      print_endline "";
      print_endline (">>>> tensor at "^(Base.intArrToString mInd)^": ");
      match x with
        | Some inst -> inst#print
        | None -> print_endline "None"
  )

let printOccupancy step = 
  step#iteri (
    fun mInd x -> 
      match x with
        | Some inst -> 
            print_string ((Base.intArrToStringShort mInd)^" ");
        | None -> ()
  );
  print_endline ""

let printNCoalsLeft step = 
  match nCoalsLeft step with
    | Some nCoals -> Printf.printf "%4d: " nCoals
    | None -> ()

let run verbosity inst = 
  let rec aux step = 
    if verbosity >= 3 then printStep step;
    if verbosity >= 1 then printNCoalsLeft step;
    if verbosity >= 2 then print_endline "";
    (* action! *)
    (* let newStep = if speed=0 then stepStep step else if speed=1 then fastStepStep verbosity step else assert(false) in *)
    let newStep = fastStepStep verbosity step in

    if onlyNone newStep then step
    else aux newStep
  in
  aux (inst#makeStep)

let postProc results = 
  Tensor.mapi (
    fun mInd -> 
      function
        | Some i -> (
            try
              Some (Array.map Obtree.reroot i#toObtreeArr)
            with 
              | Instance.IncompleteCoalescence j ->
                  failwith(
                    "Incomplete coalescence of tree "
                    ^(string_of_int j)
                    ^" in instance "
                    ^(Base.intArrToString mInd))
          )
        | None -> None
  ) results 

let stepsDiffer s1 s2 = 
  try
    Tensor.iter2 (
      fun oi1 oi2 ->
        match (oi1, oi2) with
        | (Some i1, Some i2) -> 
            if not (Instance.njForestsEqual i1 i2) then raise Exit
        | (Some i, None) -> raise Exit
        | (None, Some i) -> raise Exit
        | (None, None) -> ()
    ) s1 s2;
    false
  with
    | Exit -> true

                (*
let njTest n nTrees nSprs = 
  Printf.printf "\n\n>>>>>>>> starting njtest >>>>>>>>>\n\n";
  let betrees = Betree.yuleRsprList n nTrees nSprs in
  (* Printf.printf "tree length: %g\n" ((Betree.notRootTreeLength t1) +.
   * (Betree.notRootTreeLength t2));*)
  let dms = List.map Betree.toDistMat betrees in
  let i = Instance.linearOfDMList dms nSprs in
  let out = run 2 2 i in
  out#iteri (
    fun distArr ->
      Printf.printf "%s: " (Base.intArrToString distArr);
      function
        | Some i ->
            Array.iteri (
              fun i obtree ->
                Printf.printf "%s  " (Obtree.toString obtree)
            ) (Array.map Obtree.reroot i#toObtreeArr);
            Printf.printf "%g\n" i#getLength
        | None -> print_endline ""
  );
  let inObtrees = 
    List.map (fun t -> Obtree.reroot (Betree.toObtree t)) betrees in
  List.iter (
    fun tree -> print_endline (Obtree.toString tree)) inObtrees;
  Printf.printf "%g\n"
    (List.fold_left ( +. ) 0. (
      List.map Betree.notRootTreeLength betrees));
  ()
                 *)

(* let i = Instance.linearOfFiles ["test/test1.phymat"; "test/test2.phymat"; ]
 * 0;;  *)
