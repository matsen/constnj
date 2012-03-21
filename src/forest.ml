
class forest arg = 

  let size, treeArr = 
    match arg with
    | `OfTreeArr treeArr -> (Array.length treeArr, treeArr)
    | `OfSize n -> (n, Array.make n None)
  in

object (self)

  method getSize = size 
  method copy = new forest (`OfTreeArr (Array.copy treeArr)) (* shallow *)
  method getTreeOpt i = treeArr.(i)
  method getTree i =
    match treeArr.(i) with
      | Some t -> t
      | None -> failwith ("No tree in location "^(string_of_int i)^"!")
  method setTreeOpt i tOpt = treeArr.(i) <- tOpt
  method setTree i t = self#setTreeOpt i (Some t)
  method treePresent i = 
    match self#getTreeOpt i with
      | Some t -> true
      | None -> false
  method getTreeArr = treeArr

 (* init initializes things to be ready for coalescence *)
  method init = 
    for i=0 to size-1 do
      treeArr.(i) <- Some (Obtree.Leaf i);
    done

  method iter f = 
    Array.iter (
      function
        | Some tree -> f tree
        | None -> ()
    ) treeArr

  method getTreeList = 
    List.rev (
      Array.fold_left (
        fun soFar ->
          function
            | Some tree -> tree::soFar
            | None -> soFar
      ) [] treeArr
    )

  method nTrees = 
    let count = ref 0 in
    self#iter (fun tree -> incr count);
    !count

  method print = 
    Array.iter (
      function
        | Some tree -> Printf.printf "%s\t" (Obtree.toString tree)
        | None -> Printf.printf "-\t"
    ) treeArr;
    print_endline ""

  method printQuoted = 
    Printf.printf "[| %s |]\n" (
      String.concat "; " (
        Array.to_list (
          Array.map (
            function
              | Some tree -> Printf.sprintf "\"%s\"" (Obtree.toString tree)
              | None -> "\"\""
          ) treeArr)))

  method coalesce index1 index2 = 
    assert (index1 <> index2);
    let sind1, sind2 = Base.sortPair (index1, index2) in
    treeArr.(sind1) <- Some (Obtree.join (self#getTree sind1) (self#getTree sind2));
    treeArr.(sind2) <- None

 (* coalesceLoose: only coalesce if legal, otherwise do nothing *)
  method coalesceLoose index1 index2 = 
    if self#treePresent index1 && self#treePresent index2 && index1 <> index2 then (
      self#coalesce index1 index2
    )

(* convertMatrixIndex: go from the index of the matrix, which only has actual
 * trees, to the index wrt the forest, which has gaps.
 * *)
  method matrixToForestIndex givenMatrixInd = 
    let rec aux currInd matrixInd = 
      if currInd >= self#getSize then failwith "convertMatrixIndex: supplied index too big!"
      else (
        match treeArr.(currInd) with
          | Some tree -> 
              if matrixInd = givenMatrixInd then currInd (* found it *)
              else aux (currInd+1) (matrixInd+1) (* passed by a tree *)
          | None -> aux (currInd+1) matrixInd (* passed by a nothing *)
      )
    in 
    aux 0 0

(* forestToMatrixIndex: 
 * go from the index in the forest to the index in the matrix. return one less
 * than the number of trees which are to the left of or at the given forest
 * index.
 * *)
  method forestToMatrixIndex givenForestIndex = 
    assert(givenForestIndex < size);
    let matInd = ref (-1) in
    for i=0 to givenForestIndex do
      match treeArr.(i) with
        | Some tree -> incr matInd
        | None -> ()
    done;
    if !matInd = -1 then failwith "forestToMatrixIndex: given index too small"
    else !matInd

  
end

let equal f1 f2 = f1#getTreeArr = f2#getTreeArr

(*  *** creation *** *)

let partialYule n nSteps = 
  assert(n-nSteps >= 1);
  let f = new forest (`OfSize n) in
  f#init;
  while f#nTrees > n-nSteps do
    f#coalesceLoose (Random.int n) (Random.int n)
  done;
  f

let f = partialYule 5 2
