let constraintsOfFile fname = 
  let lines = Base.stringListOfFile fname in
  Array.of_list (
    List.map (
      fun line ->
        try
          let nums = 
            List.map int_of_string (Base.splitOnSpace line) in
          if List.length nums <> 3 then
            invalid_arg "constraintsOfFile: need three numbers per line"
          else
            ((List.nth nums 0, List.nth nums 1), List.nth nums 2) 
        with
          | Failure s -> 
              failwith ("constraintsOfFile: couldn't parse constraint file, problem with "^s^" on "^line)
    ) lines
  )

let linear nDMs maxNeighborDist = 
  let constraints = ref [] in
  for i=0 to nDMs-1 do
    for j=i+1 to nDMs-1 do
      constraints := ((i,j),maxNeighborDist*(j-i))::!constraints
    done;
  done;
  Array.of_list (List.rev !constraints)

