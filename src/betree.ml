(* have changed everything so that now taxa 1,...n are on the leaves, zero is
 * the root *)

type betree = Node of betree * betree * float
	     | Leaf of int * float

let node a b e = Node(a, b, e)
let leaf i e = Leaf(i, e)

let rec comb n = 
  if n = 1 then leaf 1 1.
  else node (comb (n-1)) (leaf n 1.) 1.

let rec balanced n = 
  let count = ref 0 in
  let round_up x = int_of_float (ceil x) in
  let round_down x = int_of_float (floor x) in
  let rec aux n = 
    if n = 1 then (
      incr count;
      leaf (!count) 1.
      )
    else
      let half = (float_of_int n) /. 2. in
      node
      (aux (round_up half)) 
      (aux (round_down half))
      1.
  in
  aux n

let rec nLeaves = function
  | Node(a,b,e) -> (nLeaves a) + (nLeaves b)
  | Leaf(i,e) -> 1

let rec getTaxa = function
  | Node(a,b,e) -> (getTaxa a) @ (getTaxa b)
  | Leaf(i,e) -> [i]

let rec depth = function
  | Node(a,b,e) -> 1 + max ( depth a) ( depth b )
  | Leaf(i,e) -> 0

let rec treeLength = function
  | Node(a,b,e) -> e +. (treeLength a) +. (treeLength b)
  | Leaf(i,e) -> e

(* notRootTreeLength: length of all edges except root edge *)
let notRootTreeLength = function
  | Node(a,b,e) -> (treeLength a) +. (treeLength b)
  | Leaf(i,e) -> 0.

let toNewick t = 
  let rec aux = function
    | Node(a,b,e) -> Printf.sprintf "(%s,%s):%g" (aux a) (aux b) e
    | Leaf(i,e) -> Printf.sprintf "%d:%g" i e
  in
  (aux t)^";"

let toNewickNoEL t = 
  let rec aux = function
    | Node(a,b,e) -> Printf.sprintf "(%s,%s)" (aux a) (aux b)
    | Leaf(i,e) -> Printf.sprintf "%d" i
  in
  (aux t)^";"

let toBothNewick t = (toNewick t)^"  "^(toNewickNoEL t)

(* checkTaxa: check that the taxa are 1,...,n. For betrees we assume that the
* root is the 0th taxon. *)
let checkTaxa t = 
  let tn = List.sort compare (getTaxa t) in
  let n = List.length tn in
  if tn <> (Base.toNList (n-1)) then failwith "tree does not have taxa 0,...,n-1"

let mapEdgeLengths f t = 
  let rec aux = function
    | Node(a,b,e) ->
        node (aux a) (aux b) (f e)
    | Leaf(i,e) ->
        leaf i (f e)
  in
  aux t

let extendRootLength extraLength = function
  | Node(a,b,e) -> Node(a, b, e +. extraLength)
  | Leaf(i,e) -> Leaf(i, e +. extraLength)

let randomizeEdgelengths randFun t = 
  mapEdgeLengths (fun e -> randFun e) t

let setRootEdgeLength t len = 
  match t with
    | Node(a,b,e) -> Node(a,b,len)
    | Leaf(i,e) -> Leaf(i,len)

let zeroEdgeLengths = randomizeEdgelengths (fun e -> 0.)

  (* make a list (i, d) where i is the leaf number and d is the number of edges
   * from that leaf to the root. don't include the root edge. *)
let depthList t = 
  let addOneToEach = function (i, d) -> (i, d+1) in
  let rec aux = function
    | Node(a,b,e) -> List.map addOneToEach ( ( aux a ) @ ( aux b ) )
    | Leaf(i,e) -> [(i,0)]
  in
  aux t

let toDistMat t = 
  checkTaxa t;
  let nLeaves = nLeaves t in
  let d = SymmMat.make nLeaves in
  (* our recursion returns the list of leaves below, and an array with the
   * distances to the leaves from the current root *)
  let rec aux = function
    | Node(a,b,e) -> 
      let (aLeaves, aDists) = aux a 
      and (bLeaves, bDists) = aux b in
      let tDists = Array.make nLeaves 0. in
      List.iter (fun i -> tDists.(i) <- e +. aDists.(i)) aLeaves;
      List.iter (fun i -> tDists.(i) <- e +. bDists.(i)) bLeaves;
      List.iter (
        fun (i, j) -> SymmMat.set d i j (aDists.(i) +. bDists.(j)) 
        ) (Base.allPairs aLeaves bLeaves);
      (aLeaves @ bLeaves, tDists)
    | Leaf(i,e) -> 
      let tDists = Array.make nLeaves 0. in
      tDists.(i) <- e;
      ([i], tDists)
  in
  let _ = aux t in
  d

let readEdgeLength s = 
  try
    float_of_string s 
  with
      Failure ("float_of_string") -> 
         failwith ("readEdgeLength could not parse "^s)

let cleanWhiteSpace = Pcre.replace ~pat:"\\s*" ~templ:""

(* our recursive regexp. *)
(* (?: is a non-capturing group *)
let treeRegexp = 
  Pcre.regexp ~study:true
    "\\((?P<b>(?>[^(),]+)|(?:\\((?P>b),(?P>b)\\)(?::[\\d\\.]*)?)),((?P>b))\\)"

let isBadLeafName s = Pcre.pmatch ~pat:"[(),:]" s

(* ofNewick just strips off the edgelength and then hands things over to
 * ofNewickNoEL *)
(* note WRT indexing below... a.(0) is entire match when using full_match:true,
 * then split bits *)
let rec ofNewick s = 
  try
    let a = Pcre.extract ~full_match:false ~pat:"(.*):([\\d\\.]*)" s in
    ofNewickNoEL a.(0) (readEdgeLength a.(1))
  with
  | Not_found -> failwith "need an edge length!"

and ofNewickNoEL s e = 
  try
    let a = Pcre.extract ~full_match:false ~rex:treeRegexp s in
    Node (ofNewick a.(0), ofNewick a.(1), e)
  with
    | Not_found -> 
        if isBadLeafName s then failwith (s^" is not an ok leaf name.");
        (* print_endline s; *)
        Leaf(int_of_string (cleanWhiteSpace s),e)

(* assumes one tree per line *)
let ofFile fname = 
  let in_ch = open_in fname in
  let trees = ref [] in
  try
    while true do trees := (ofNewick (input_line in_ch))::!trees done;
    List.rev !trees
  with 
     | End_of_file -> List.rev !trees 

(* rspr:
 * rTree is the tree to be removed, and edge is the place to put it.
 *)
let rspr baseTree rTree edge = 
  let treesMatch t1 t2 = zeroEdgeLengths t1 = zeroEdgeLengths t2 in
  let rec perhapsModify t = 
    if treesMatch t edge then (
  (* we want to add rTree here. split the incoming edge uniformly *)
      let w = Random.float 1. in
      match t with
        | Node(a, b, edgeBL) ->
           Node(rTree, 
                recur (Node(a, b, (1.-.w) *. edgeBL)),
                w *. edgeBL)
        | Leaf (i, edgeBL) -> 
           Node(rTree, Leaf(i, (1.-.w) *. edgeBL), w *. edgeBL)
    )
    else recur t
  and recur = function
      | Node(a,b,e) -> 
      (* check if one of the edges is something we want to delete *)
        if treesMatch a rTree then 
  (* we delete the edge going to a, and add the two descending edge lengths *)
          extendRootLength e (perhapsModify b)
        else if 
  (* same for b *) 
          treesMatch b rTree then extendRootLength e (perhapsModify a)
        else Node(perhapsModify a, perhapsModify b, e)
      | Leaf(i,e) -> Leaf(i,e) (* if we have gotten to here we don't want to delete it *)
  in
  perhapsModify baseTree

(* let testRspr b r e = (toNewick b, toNewick (rspr b r e))*)

module OrderedBetree = 
struct
  type t = betree
  let compare = Pervasives.compare
end

module BetreeSet = Set.Make(OrderedBetree)

let uniformFromBetreeSet s = Base.list_uniformSelection (BetreeSet.elements s)

(* this version of yule roots everything at the outgroup zero *)
let yule randFun n = 
  assert(n>0);
  let s = ref BetreeSet.empty in
  for i=1 to n-1 do
    s := BetreeSet.add (Leaf (i, randFun ())) !s
  done;
  let removeUnif () = 
    let x = uniformFromBetreeSet !s in
    s := BetreeSet.remove x !s;
    x
  in
  while BetreeSet.cardinal !s > 1 do 
    let a = removeUnif () 
    and b = removeUnif () in
    s := BetreeSet.add (Node(a,b, randFun ())) !s
  done;
  (Node(Leaf (0, randFun ()), List.hd (BetreeSet.elements !s), randFun ()))

let unifYule = yule (fun () -> Random.float 0.1)

let toEdgeSet tree = 
  let s = ref BetreeSet.empty in
  let rec aux t = 
    s := BetreeSet.add t !s;
    match t with
      | Node(a,b,e) -> aux a; aux b
      | Leaf(i,e) -> ()
  in
  aux tree;
  !s

(* findRsprPair: we return a possible subtree to move, and where we might move
* it to. The BetreeSet.diff means that we don't choose a location to move which
* is part of the thing getting moved. *)
let findRsprPair tree = 
  (* start with the non-root edges *)
  let s = BetreeSet.remove tree (toEdgeSet tree) in
  let rec aux () = 
    let anEdge = uniformFromBetreeSet s in
    match anEdge with
      | Node(a, b, e) -> anEdge
                           (* don't pick root to move *)
      | Leaf(index, e) -> if index = 0 then aux () else anEdge
  in
  let r = aux () in
  (r, uniformFromBetreeSet (BetreeSet.diff s (toEdgeSet r)))
 
let randomRspr randFun tree = 
  let (r, e) = findRsprPair tree in
  (* Printf.printf "moving %s\n" (toNewickNoEL r);*)
  rspr tree r e
 
let testRandRspr randFun tree = 
  let (r, e) = findRsprPair tree in
  (toNewick tree, toNewick r, toNewick e, toNewick (rspr tree r e))
 
let perturbTree randFun = 
  randomizeEdgelengths (fun e -> 0.5 *. (e +. randFun ())) 
   
(* yuleRsprList: 
 * each tree in the list is some given number of sprs from the previous one,
 * along with a perturbation as specified above 
 *)
let yuleRsprList randFun n nTrees nSprs = 
  let rec genSprs t nSprsLeft =  
    if nSprsLeft <= 0 then t
    else randomRspr randFun (genSprs t (nSprsLeft-1))
  in
  let rec genList t nTreesLeft =
    if nTreesLeft = 0 then []
    else t::(genList (genSprs (perturbTree randFun t) nSprs) (nTreesLeft-1))
  in
  genList (yule randFun n) nTrees
    
let yuleRsprDMPair randFun n = 
  let t = yule randFun n in
  (toDistMat t, toDistMat (randomRspr randFun t))

let rec toObtree = function
  | Node(a,b,e) -> Obtree.join (toObtree a) (toObtree b)
  | Leaf(i,e) -> Obtree.Leaf i

let toObtreeStr t = Obtree.toString (toObtree t)

let beTreeListToFile fname betl = 
  Base.stringListToFile fname (List.map toNewick betl)

(*
let rng = Base.makeRng 0
let ourExp () = Gsl_randist.exponential rng (0.1 /. (float_of_int (40-1)))
let trees = yuleRsprList ourExp 40 3 1
let () = beTreeListToFile "xx.tre" trees
*)

let constRandFun e = 5.

let b = ofNewick "((3:1.,4:1.):1.,(2:1.,(5:1.,1:1.):10.):1.):1";;
let r = ofNewick "4:1.";;
let e = ofNewick "(5:1.,1:2.):10.";;

let x = testRandRspr constRandFun b;;
let x = rspr b r e
