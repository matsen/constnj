type relationship = Equal | Subset | Superset | Disjoint | Incompatible

exception GoodPartition of int array

module OrderedInt = 
struct
  type t = int
  let compare = ( - )
  (* subtraction appears to be faster than polymorphic compare *)
end

module OrderedIntArr = 
struct
  type t = int array
  let compare = Pervasives.compare
end

module IntSet = Set.Make(OrderedInt)
module IntSetSet = Set.Make(IntSet)
module IntArrSet = Set.Make(OrderedIntArr)

let rec ofList = function
  | x::l ->
      if l = [] then IntSet.singleton x
      else IntSet.add x (ofList l)
  | [] -> failwith "Rsplit.ofList: passed empty list!"

let taxa = IntSet.elements
let add = IntSet.add
let union = IntSet.union
let singleton = IntSet.singleton
let setMinus = IntSet.diff
let size = IntSet.cardinal
let nTaxaSet nTaxa = ofList (Base.toNList (nTaxa-1))
let map f s = ofList (List.map f (IntSet.elements s))

let reroot allTaxaSet set = 
  if IntSet.mem 0 set then setMinus allTaxaSet set
  else set

let rerootList allTaxaSet setList = 
  List.map (fun set -> reroot allTaxaSet set) setList

let toString rsplit = 
  "{"^(String.concat ";" (List.map string_of_int (taxa rsplit)))^"}"

let unionOverList = function
  | x::l -> List.fold_left IntSet.union x l
  | [] -> IntSet.empty

(* intersect: returns if two rsplits intersect *)
let intersect a b = 
  try
    IntSet.iter (
      fun x -> if IntSet.mem x a then raise Exit
    ) b;
    false
  with
    | Exit -> true

let findRelationship a b =
  if intersect a b then (
    if IntSet.equal a b then Equal
    else if IntSet.subset a b then Subset
    else if IntSet.subset b a then Superset
    else Incompatible
  )
  else Disjoint

(* compatible: perhaps later replace with something more efficient? *)
let compatible a b = findRelationship a b <> Incompatible

let rsplitList_uniques l = 
  IntSetSet.elements (
    List.fold_left (fun s x -> IntSetSet.add x s) IntSetSet.empty l)

let printSet x = 
  List.iter (fun y -> Printf.printf "%d " y) (taxa x);
  print_endline ""

let printRsplitList l = 
  List.iter (
    fun rs -> Printf.printf "%s " (toString rs);
  ) l;
  print_endline ""

let printIntSetSet s = 
  printRsplitList (IntSetSet.elements s)



let addSplit split partitionSet = 
  let newPartSet = ref partitionSet in
  List.iter (
    fun partSet ->
      let isect = IntSet.inter split partSet in
      if not (IntSet.is_empty isect) then (
        let diff = IntSet.diff partSet isect in
        if not (IntSet.is_empty diff) then (
          (* split partitions partSet into two. split it up. *)
          newPartSet := IntSetSet.remove partSet !newPartSet;
          newPartSet := IntSetSet.add isect !newPartSet;
          newPartSet := IntSetSet.add diff !newPartSet;
        )
      )
  ) (IntSetSet.elements partitionSet);
  !newPartSet

let trivialPartition n = 
  IntSetSet.singleton (ofList (Base.toNList (n-1)))

(* preInfimum returns the sets of the partition *)
let preInfimum nTaxa rsplitList = 
  List.fold_left (
    fun partSoFar newSplit ->
      addSplit newSplit partSoFar
  ) (trivialPartition nTaxa) rsplitList

let infimum nTaxa rsplitList = 
  let count = ref 0 in
  let partition = Array.make nTaxa 0 in
  IntSetSet.iter (
    fun set ->
      IntSet.iter (
        fun taxon ->
          partition.(taxon) <- !count
      ) set;
      incr count
  ) (preInfimum nTaxa rsplitList);
  partition

let partIntArrOfIntSetSet nTaxa intSetSet = 
  let count = ref 0 in
  let partition = Array.make nTaxa 0 in
  IntSetSet.iter (
    fun set ->
      IntSet.iter (
        fun taxon ->
          partition.(taxon) <- !count
      ) set;
      incr count
  ) intSetSet;
  partition


(* iterOverCompatPartitions:
 * k is the number of splits we want to add to the start partition
 * *)
let iterOverCompatPartitions f k startPartition splitList = 
  let expectedNSets = k + IntSetSet.cardinal startPartition in
  let rec aux kRemaining partitionSoFar availSplits = 
    assert(kRemaining >= 0);
    if kRemaining = 0 then (
      let nSets = IntSetSet.cardinal partitionSoFar in
      if expectedNSets < nSets then (
        printIntSetSet partitionSoFar;
        failwith (
          "iterOverCompatPartitions: incompatible sets? Expected "
          ^(string_of_int expectedNSets)
          ^" got "
          ^(string_of_int nSets))
      )
    (* below: note that we can have expectedNSets < nSets, and we should just
     * throw those out. e.g. splitting off (1,2), then 1, then 2. *)
      else if expectedNSets = nSets then
        f partitionSoFar
    )
    else
      match availSplits with
        | split::restAvail -> 
            (* first add to the partition *)
            aux (kRemaining-1) (addSplit split partitionSoFar) restAvail;
            (* then forget about it and move on *)
            aux kRemaining partitionSoFar restAvail
        | [] -> ()
  in
  aux k startPartition splitList

(* iterOverIntSetPartitions:
 * (IntSetSet.t -> unit) -> IntSet.elt list -> int -> unit = <fun>
 * given an intList and a maxPartSize, apply f to the IntSetSets which are the
 * partitions of intList into partns with at most maxPartSize sets.
 *)
let iterOverIntSetPartitions f intList maxPartSize = 
  let rec aux part = function
    | x::rest -> 
        IntSetSet.iter (
          fun s ->
            aux (
              IntSetSet.add (IntSet.add x s) (
                IntSetSet.remove s part) ) rest;
        ) part;
        if IntSetSet.cardinal part < maxPartSize then
          aux (IntSetSet.add (IntSet.singleton x) part) rest
    | [] -> f part
  in
  aux IntSetSet.empty intList

let setSizeAtLeast minSize part = 
  try
    IntSetSet.iter (
      fun s ->
        if IntSet.cardinal s < minSize then raise Exit
    ) part;
    true
  with
    | Exit -> false

let countPartitions minSetSize initSetSize =
  let nParts = ref 0 in
  iterOverIntSetPartitions (
    fun s -> 
      if setSizeAtLeast minSetSize s then nParts := !nParts + 1)
                           (Base.toNList (initSetSize-1))
                           max_int;
  !nParts


let findAllPartitionsFromKTuples nTaxa k rsplitList = 
  let parts = ref IntArrSet.empty in
  List.iter (
    fun tuple ->
      parts := 
        IntArrSet.add (infimum nTaxa tuple) !parts
  ) (Base.kTuplesOfList rsplitList k);
  IntArrSet.elements !parts

(* the partitions "m-value", i.e. one less than the part size *)
let partitionMVal a = 
  assert(Array.length a > 0);
  Array.fold_left max a.(0) a

let partitionSize a = 1+(partitionMVal a)

let findAllMPartitions nTaxa m rsplitList = 
  assert(m>=0);
  if m=0 then [Array.make nTaxa 0] 
  else (
    let parts = ref IntArrSet.empty in
    for tupleSize=1 to m do
      List.iter (
        fun tuple ->
          let newPart = infimum nTaxa tuple in
          if partitionMVal newPart = m then
            parts := IntArrSet.add newPart !parts
      ) (Base.kTuplesOfList rsplitList tupleSize)
    done;
    IntArrSet.elements !parts
  )

let memoryIntensiveFindGoodMPartition isGood nTaxa m rsplitList = 
  assert(m>=0);
  let checkPartition partition = 
    if isGood partition then raise (GoodPartition partition)
  in
  try
    List.iter checkPartition (findAllMPartitions nTaxa m rsplitList);
    None
  with
    | GoodPartition partition -> Some partition

let findGoodMPartition isGood nTaxa m rsplitList = 
  assert(m>=0);
  let checkPartition partition = 
    if isGood partition then raise (GoodPartition partition)
  in
  try
    if m=0 then checkPartition (Array.make nTaxa 0)
    else (
      for tupleSize=1 to m do
        Base.iterOverKTuplesOfList (
          fun tuple ->
            let newPart = infimum nTaxa tuple in
            if partitionMVal newPart = m then checkPartition newPart
        ) rsplitList tupleSize
      done;
    );
    None
  with
    | GoodPartition partition -> Some partition

let partitionToString partition = 
  "["^(String.concat ";" (Array.to_list (Array.map string_of_int partition)))^"]"

let testPartSet l = infimum 10 (List.map ofList l) 

(* note that for later, it's important to have the sets in increasing order *)
let setListOfPartition partition = 
  let taxonListArr = Array.make (Array.length partition) [] in
  Array.iteri (
    fun taxon splitVal -> 
      taxonListArr.(splitVal) <- taxon::taxonListArr.(splitVal)
  ) partition;
  List.rev (
    List.map List.rev (
      Array.fold_left (
        fun soFar taxonList -> 
          if taxonList = [] then soFar else taxonList::soFar
      ) [] taxonListArr))

let intSetListOfPartition partition = 
  List.map ofList (setListOfPartition partition)

let partitionCompatWRsplitList partition rsplitList =
  try
    List.iter (
      fun partRsplit ->
        List.iter (
          fun rsplit -> 
            if not (compatible partRsplit rsplit) then raise Exit
        ) rsplitList
    ) (intSetListOfPartition partition);
    true
  with
    | Exit -> false

let prettyPrint ff y = 
  Format.open_box 0;
  Format.fprintf ff "{";
  List.iter (
    fun x ->
      Format.fprintf ff "%d;" x;
      Format.print_space ();
  ) (taxa y);
  Format.fprintf ff "}";
  Format.close_box ()
         (*
let a = Rsplit.ofList [0;1];;
let b = Rsplit.ofList [3;4];;
let c = ofList [0;1;2];;
let d = ofList [0;3];;
let e = ofList [0;3];;
let maxima = findMaxima relMat taxList
let part = findPartition 5 arr relMat taxList

let part = preInfimum 7 (List.map ofList [[5; 6; ]; [3; 4; 5; 6; ]; [1; 2; 3; 4; 5; 6; ]])
let part = preInfimum 4 (List.map ofList [[3; ]; [1; 2; 3; ]])
let part = preInfimum 4 (List.map ofList [[2; ]; [3; ]; [1;2;3] ])
let part = preInfimum 3 (List.map ofList [[1; ]; [2; ]; [1;2] ])


let printPartitions = 
  iterOverCompatPartitions (fun s -> printRsplitList (IntSetSet.elements s))

let test nTaxa k listList = 
  printPartitions k (trivialPartition nTaxa) (List.map ofList listList)

let part = preInfimum 7 (List.map ofList [[1; 2; 3; ]]);;
printRsplitList( IntSetSet.elements part);;

let gart = addSplit (ofList [1]) part;;
printRsplitList( IntSetSet.elements gart);;

let rart = test 5 2 [[1]; [1;2;3]]
let x = printPartitions 2 part (List.map ofList [[3]; [4;5]])

           (fun ss -> if setSizeAtLeast 2 ss then printRsplitList (IntSetSet.elements ss))

let () = iterOverIntSetPartitions 
           (fun ss -> printRsplitList (IntSetSet.elements ss))
           [0; 1; 2; 3; ]
           200

          *)


(*

let printPartitions = 
  iterOverCompatPartitions (fun s -> printRsplitList (IntSetSet.elements s))

let test nTaxa k listList = 
  printPartitions k (trivialPartition nTaxa) (List.map ofList listList)

let part = preInfimum 7 (List.map ofList [[1; 2; 3; ]]);;
printRsplitList( IntSetSet.elements part);;

let gart = addSplit (ofList [1]) part;;
printRsplitList( IntSetSet.elements gart);;

let rart = test 5 1 [[1]; [1;2;3]];;

let x = printPartitions 2 part (List.map ofList [[3]; [4;5]])

          (*
(fun ss -> if setSizeAtLeast 2 ss 
 then printRsplitList (IntSetSet.elements ss))
           *)

let () = iterOverIntSetPartitions 
           (fun ss -> printRsplitList (IntSetSet.elements ss))
           [0; 1; 2; 3; ]
           200
 *)

