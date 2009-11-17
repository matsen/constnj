type obtree = Node of obtree * obtree * int
	     | Leaf of int

let getIndex = function
  | Node(a, b, i) -> i
  | Leaf(i) -> i

let compare a b = Pervasives.compare (getIndex a) (getIndex b) 

let join a b = 
  if compare a b = -1 then
    Node(a, b, getIndex a)
  else
    Node(b, a, getIndex b)

let rec comb n = 
  if n = 1 then Leaf ( 0 )
  else join (comb (n-1)) (Leaf (n-1))

let rec combOfList = function
  | x::l -> 
      if List.length l > 0 then join (combOfList l) (Leaf x)
      else Leaf x
  | [] -> assert(false)

let rec balanced n = 
  let count = ref 0 in
  let round_up x = int_of_float (ceil x) in
  let round_down x = int_of_float (floor x) in
  let rec aux n = 
    if n = 1 then (
      incr count;
      Leaf (!count - 1)
      )
    else
      let half = (float_of_int n) /. 2. in
      join
      (aux (round_up half)) 
      (aux (round_down half))
  in
  aux n

let rec nLeaves = function
  | Node(a,b,i) -> (nLeaves a) + (nLeaves b)
  | Leaf(i) -> 1

let rec depth = function
  | Node(a,b,i) -> 1 + max ( depth a) ( depth b )
  | Leaf(i) -> 0

let rec toString = function
  | Node(a,b,i) -> Printf.sprintf "(%s,%s)" (toString a) (toString b)
  (* | Node(a,b,i) -> Printf.sprintf "(%s,%s|%d)" (toString a) (toString b) i*)
  | Leaf(i) -> Printf.sprintf "%d" i

let toNamedString nameArr tree = 
  let rec aux = function
    | Node(a,b,i) -> Printf.sprintf "(%s,%s)" (aux a) (aux b)
    | Leaf(i) -> nameArr.(i)
  in
  try
    (aux tree)^";"
  with
    | Invalid_argument s -> ("nameArr not big enough... "^s)

let rec getTaxa = function
  | Node(a, b, i) -> (getTaxa a)@(getTaxa b)
  | Leaf(i) -> [i]

let print t = Printf.printf "%s\n" ( toString t )

(* our recursive regexp. *)
(* (?: is a non-capturing group *)
let treeRegexp = 
  Pcre.regexp ~study:true
    "\\((?P<b>(?>[^(),]+)|(?:\\((?P>b),(?P>b)\\))),((?P>b))\\)"

(* note WRT indexing below... a.(0) is entire match when using full_match:true,
 * then split bits *)
let rec ofNewick s = 
  try
    let a = Pcre.extract ~full_match:false ~rex:treeRegexp s in
    join (ofNewick a.(0)) (ofNewick a.(1))
  with
    | Not_found -> 
        try
          Leaf (int_of_string s) 
        with
        | Failure c -> failwith ("Obtree.ofNewick: taxon label not integer: "^s)

(* checkTaxa: check that the taxa are 0,...,n-1 *)
let checkTaxa t = 
  let tn = List.sort Pervasives.compare (getTaxa t) in
  let n = List.length tn in
  if tn <> Base.toNList (n-1) then failwith "tree does not have taxa 0,...,n-1"

let addOutgroup t = Node(Leaf 0, t, 0)

let obtreeArrOfNewickFile fname = 
  Array.of_list (
    List.map ofNewick (
      Base.stringListOfFile fname))

(* reroot:
 * we look for the outgroup's sister tree, which will be called ogs, and the
 * accumulated tree list, which are the list of trees on the path going from the
 * outgroup to the root of the tree *)
let reroot tree = 
  let rec aux = function
    | Node(a1, a2, i) -> 
        if a1 = Leaf 0 then (Some a2, []) (* we have hit outgroup *)
        else 
          let (ogsOpt1, atl1) = aux a1 
          and (ogsOpt2, atl2) = aux a2 in (
            match ogsOpt1, ogsOpt2 with
              | Some ogs1, Some ogs2 -> failwith "reroot: root appears twice!"
              | Some ogs1, None -> Some ogs1, a2::(atl1)
              | None, Some ogs2 -> Some ogs2, a1::(atl2)
              | None, None -> None, []
          )
    | Leaf x -> None, []
  in
  let ogsOpt, atl = aux tree in
  (* below: we want to coalesce the two nodes closest to the root if they exist *)
  let modAtl = 
    match atl with 
      | first::fRest -> (
          match fRest with
            | second::sRest -> (join first second)::sRest 
            | [] -> atl
        )
      | [] -> atl
  in
  match ogsOpt with
    | Some ogs -> (
        match modAtl with
          | base::rest ->
              join (Leaf 0) ( join ogs (
                  List.fold_left (fun soFar t -> join soFar t) base rest))
          | [] -> join (Leaf 0) ogs
      )
    | None -> failwith "reroot: outgroup not found!"

(* toCoalSeq *)
let toCoalSeq obtree = 
  let rec aux = function
    | Node(a, b, i) -> 
        (Base.sortPair (getIndex a, getIndex b))::((aux a) @ (aux b))
    | Leaf(i) -> []
  in
  List.rev (aux obtree)

(* toEdgeRsplitList: make a list of rSplits associated with an obtree. designed
 * such that the first element of the rSplit list is the root split. *)
let toEdgeRsplitList tree = 
  let rec aux = function
    | Node(a, b, i) -> 
        let aRSplits = aux a
        and bRSplits = aux b in
        assert(aRSplits <> [] && bRSplits <> []);
        (Rsplit.union (List.hd aRSplits) (List.hd bRSplits))
        ::(aRSplits @ bRSplits)
  | Leaf(i) -> [Rsplit.singleton i]
  in
  List.filter (
    fun set -> not (Rsplit.IntSet.is_empty set)
  ) (Rsplit.rerootList (Rsplit.nTaxaSet (nLeaves tree)) (aux tree))

let toEdgeRsplitSet tree = 
  List.fold_left 
    (fun s elt -> Rsplit.IntSetSet.add elt s)
    Rsplit.IntSetSet.empty 
    (toEdgeRsplitList tree)

let rfDistance t1 t2 = 
  assert(nLeaves t1 = nLeaves t2);
  let s1 = toEdgeRsplitSet t1
  and s2 = toEdgeRsplitSet t2 in
  let i = Rsplit.IntSetSet.inter s1 s2 in
  let n1 = Rsplit.IntSetSet.cardinal (Rsplit.IntSetSet.diff s1 i) in
  let n2 = Rsplit.IntSetSet.cardinal (Rsplit.IntSetSet.diff s2 i) in
  assert(n1 = n2);
  n1


(*
 let t1 = ofNewick "(0,(((3,(1,2)),(5,6)),4))";;
 let t2 = ofNewick "(0,(((5,(1,2)),(3,6)),4))";;
 let t = ofNewick "((((0,(1,5)),(2,6)),3),4)";;
      *)
