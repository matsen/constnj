
let dmAndNamesOfPhylipChannel ch = 
  let x = input_line ch in
  let length = 
    try
      match Base.splitOnSpace x with
        | [] -> raise (Failure "")
        | [lenStr] -> int_of_string lenStr
        | x::l -> raise (Failure "")
    with
      | Failure failStr -> 
          failwith ("Is this a phylip DM file? First line should just be a single number."^failStr)
  in
  let names = ref [] in
  let rows = ref [] in
  let currRow = ref [] in
  let finishRow () = 
    match !currRow with
      | [] -> failwith "parsing problem: empty line?"
      | name::vals ->
          try
            names := (!names) @ [name];
            rows := !rows @ [List.map float_of_string vals];
            currRow := [];
          with
            | Failure failStr -> failwith ("parsing problem: "^failStr)
  in
  let rec gobble () = 
    currRow := !currRow @ (Base.splitOnSpace (input_line ch));
    if List.length !currRow <= length then gobble ()
    else finishRow ()
  in
  try
    while true do gobble () done;
    assert(false) (* should never get here... should raise an End_of_file *)
  with 
    | End_of_file -> 
        (* where we really end up *)
        if !currRow <> [] then finishRow (); 
        try
          if List.length !rows <> length then raise Exit;
          if List.length !names <> length then raise Exit;
          List.iter (
            fun row -> 
              if List.length row <> length then raise Exit;
          ) !rows;
          (Array.of_list !names, Array.of_list (List.map Array.of_list !rows))
        with
          | Exit -> failwith ("matrix not expected length: expecting "^(string_of_int length))

let dmAndNamesOfPhylipFile fname = 
  let ch = open_in fname in
  let dm = dmAndNamesOfPhylipChannel ch in
  close_in ch;
  dm

(* let x = dmOfPhylipFile "envDM.phymat";;*)

