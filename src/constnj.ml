let verbosity = ref 2
(* let speed = ref 1 *)
let linearDist = ref 1 
let constrFile = ref ""
let runName = ref ""
let numberedTrees = ref false

let parse_args () =
  let files  = ref []
  and v = "-v", Arg.Set_int verbosity,
   "Set the verbosity level. 0 is no status updates, 1 is some, 2 is more, and 3 is puking. Default is 2."
     (*
  and s = "-s", Arg.Set_int speed,
   "Speed. As described in the manual, zero is slow, and one is faster, though may not return as optimal of an answer as zero. "
      *)
  and l = "-l", Arg.Set_int linearDist,
   "The maximal SPR distance allowed between the two trees, assuming distance matrices are constrained in a linear fashion. Overridden by the -c option."
  and c = "-c", Arg.Set_string constrFile,
   "Supply a constraint file."
  and o = "-o", Arg.Set_string runName,
   "Name this run for outfiles. Default is basename of first DM file name."
  and n = "-n", Arg.Set numberedTrees,
   "Write output trees in terms of numbers rather than names."
  in
  let usage =
    "constnj\n"
  and anon_arg arg =
    files := arg :: !files in
  let args = [ v; l; c; o; n ] in
  Arg.parse args anon_arg usage;
  List.rev !files
     
    (* note return code of 0 is OK *)
let () =
  if not !Sys.interactive then (
    let fNames = parse_args () in

    try
      (* if !speed < 0 || !speed > 1 then failwith ("Speed "^(string_of_int !speed)^" is not legal.");*)
      if fNames = [] then raise (Sys_error "please supply some distance matrices.");
      if !runName = "" then runName := Base.basename (List.hd fNames);
      let info = List.map ParseDM.dmAndNamesOfPhylipFile fNames in
      let names = Array.of_list (List.map fst info)
      and constraints = 
        if !constrFile <> "" then
          Constraints.constraintsOfFile !constrFile
        else
          Constraints.linear (List.length info) !linearDist
      and dms = Array.of_list (List.map snd info)
      in

      let lengthsCh = open_out (!runName^".lengths") in

      (* first do independent construction *)
      
      (* fst is length, snd is tree *)
      let indepRun = Array.map NjForest.runNJOnDM dms in
      Printf.fprintf lengthsCh "indep: %g\n" 
        (Array.fold_left ( +. ) 0. (Array.map fst indepRun));
      let outCh = open_out (Printf.sprintf "%s.indep.tre" !runName) in
      Array.iteri (
        fun i obtree ->
          if !numberedTrees then
            Printf.fprintf outCh "%s\n" (Obtree.toString obtree)
          else
            Printf.fprintf outCh "%s\n" (Obtree.toNamedString names.(i) obtree)
      ) (Array.map snd indepRun);
      close_out outCh;

      (* run usual distrec *)
      let results = Step.run !verbosity (
        new Instance.instance 
          (`OfDMsAndForestIndPairLimits (dms, constraints))) in

      if !verbosity >= 3 then Step.printStep results;

      let toWrite = ref [] in
      results#iteri (
        fun distArr ->
          function
            | Some inst -> 
                toWrite := (Base.intArrToString distArr, inst#getLength)::(!toWrite)
            | None -> ()
      );
      List.iter (fun (s, l) -> Printf.fprintf lengthsCh "%s: %g\n" s l)
        (List.sort (fun (s1, l1) (s2, l2) -> compare l1 l2) !toWrite);

      close_out lengthsCh;

      let trees = Step.postProc results in
      trees#iteri (
        fun distArr ->
          function
            | Some treeArr ->
                let outCh = 
                  open_out (
                    Printf.sprintf "%s.%s.tre" !runName 
                      (String.concat "_" (
                        List.map string_of_int (Array.to_list distArr)))) in
                Array.iteri (
                  fun i obtree ->
                    if !numberedTrees then
                      Printf.fprintf outCh "%s\n" (Obtree.toString obtree)
                    else
                      Printf.fprintf outCh "%s\n" (Obtree.toNamedString names.(i) obtree)
                ) treeArr;
                close_out outCh;
            | None -> ()
      );

      if !verbosity >= 1 then 
        Printf.printf "elapsed time: %s\n" (Base.timeStrOfSeconds (Sys.time ()));
      exit 0

    with Sys_error msg -> prerr_endline msg; exit 2
  )
