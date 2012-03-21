
let empty_or_comment line = 
  (Pcre.pmatch ~pat:"^\\s*$" line) ||
  (Pcre.pmatch ~pat:"^\\s*#" line) 

let not_empty_or_comment line = not (empty_or_comment line) 

let ofFiles fname_list = 
  let ih = Hashtbl.create 5 in (* integers *)
  let fh = Hashtbl.create 5 in (* floats *)
  let bh = Hashtbl.create 5 in (* bools *)
  let sh = Hashtbl.create 5 in (* strings *)
  let eh = Hashtbl.create 5 in (* existence - if defined or not *)
  List.iter (
    fun fname ->
      let cin = open_in fname in 
      let rec readlines acc = 
	try readlines ((input_line cin)::acc)
	with End_of_file -> close_in cin; List.rev acc
      in
      let no_space = Pcre.replace ~pat:"\\s" ~templ:"" in
      List.iter (
	fun str -> 
	  if not (empty_or_comment str) then (
	    let info = Pcre.split ~pat:"\\s:\\s" str in
	    let name = List.nth info 0 
	    and var_type = List.nth info 1
	    and v_str = List.nth info 2
	    in
	    
	    let clean_type = no_space var_type in
	    let clean_var = no_space v_str in
	    if clean_type = "i" then 
	      Hashtbl.add ih (no_space name) (int_of_string clean_var)
	    else if clean_type = "f" then 
	      Hashtbl.add fh (no_space name) (float_of_string clean_var)
	    else if clean_type = "b" then 
	      Hashtbl.add bh (no_space name) (bool_of_string clean_var)
	    else if clean_type = "s" then 
	      Hashtbl.add sh (no_space name) clean_var
	    else 
	      failwith "unknown variable type\n";
	    Hashtbl.add eh (no_space name) true;
	  )
      ) (readlines []);
  ) fname_list;
  let make_fun h = 
    fun str -> 
      try Hashtbl.find h str with
	  Not_found -> failwith (str^" not found in ctl file!\n")
  in 
  let existence_fun str = 
    try Hashtbl.find eh str with
	Not_found -> false
  in
  (make_fun ih, make_fun fh, make_fun bh, make_fun sh, existence_fun)

let find_ctls () = 
  List.map 
    ( ( ^ ) ((Sys.getcwd ())^"/") )
    (List.filter 
       (Pcre.pmatch ~pat:"\\.ctl$") 
       (Array.to_list (Sys.readdir ".") ) )

(* find all the control files in the directory list. relative
   directory names, e.g. ".." are fine.
*)
let find_all_ctls dir_list = 
  let cwd = Sys.getcwd () in
  List.flatten (
    List.map (
      fun dir ->
	Sys.chdir dir;
	let ctls = find_ctls () in
	Sys.chdir cwd;
	ctls
    ) dir_list 
  )



    (*
let (ih, fh, bh, sh, existence_fun) = ofFiles ["spaz.ctl"]
     *)
