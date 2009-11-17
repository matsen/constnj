let verbose = ref false

let parse_args () =
  let files  = ref [] in
  let v = "-v", Arg.Set verbose,
    "Verbose: print out input and edges cut"
  in
  let usage =
    "rspr: implements the Bordewich-Semple algorithm to calculate rSPR\n"
  and anon_arg arg =
    files := arg::!files in
  let args = [ v ] in
  Arg.parse args anon_arg usage;
  List.rev !files
     
let process_file file name =
  let ret_code = ref 0 in
  try
    let treeStr1 = input_line file 
    and treeStr2 = input_line file in
    let (gf1, gf2) = GhostForestFuns.gfOfObtreePair 
                       (Obtree.ofNewick treeStr1) 
                       (Obtree.ofNewick treeStr2) in
    let partition = MAP.find gf1 gf2 in
    let mVal = Rsplit.partitionMVal partition in
    if !verbose then (
      print_endline "original strings:";
      print_endline treeStr1;
      print_endline treeStr2;
      print_endline "ghost forests:";
      gf1#print;
      gf2#print;
    );
    if !verbose then (
      Printf.printf "\npartition with %s:\n" (Rsplit.partitionToString partition);
    );
    if !verbose then
      Printf.printf "SPR distance: %d\n" mVal
    else
      Printf.printf "%d\n" mVal;
    !ret_code
  with 
    | Exit -> 0 
    | End_of_file -> 0

    (* note return code of 0 is OK *)
let () = (
  if not !Sys.interactive then (

    let files = parse_args () in

    if files = [] then exit 0;

    let collect ret_code filename =
      try
        let file = open_in filename in
        let frc = process_file file filename in
        close_in file;
        if frc = 0 && ret_code = 1 then 0 else ret_code
      with Sys_error msg -> prerr_endline msg; 2 in
    exit (List.fold_left collect 1 files)

  )
)
