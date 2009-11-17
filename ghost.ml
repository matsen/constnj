(* 
 * CT = contracted tree
 * CC = contracted chain
 * terminal = CT or CC
 * we enforce an ordering for the branchings of the tree with the smallest
 * always on the left. 
 *
 * Note that in a single coalescence event, we can have either a CT event, or a
 * CC event, but not one and then the other. Can't have CT -> CC, because we
 * must already have isomorphic subtrees in order to have a CC. Can't have a CC
 * -> CT, because isomorphic contractible chains are already isomorphic. 
 *
 * an "extendo" is a leaf/CT sibling to a CC. i.e. a CC ready to get extended.
 *
 * the label of a CC is the minimum of the labels of the leaves it contracts
 * *)

exception PartitionIncompatible

module IS = Rsplit.IntSet

type ghost = Node of ghost * ghost * int
             | CC of ghost * int
             | CT of int
             | Leaf of int

(* getIndex:
 * the miniumum leaf label of the tree
 * *)
let rec getIndex = function
  | Node(a, b, i) -> i
  | CC(a, i) -> min (getIndex a) i
  | CT(i) -> i
  | Leaf(i) -> i

let containsRoot t = 
  getIndex t = 0

let compare a b = Pervasives.compare (getIndex a) (getIndex b) 

let join a b = 
  let minI = min (getIndex a) (getIndex b) in
  if compare a b = -1 then Node ( a, b, minI )
  else Node ( b, a, minI )

(* joinOpt: the idea of this function is that it joins two things, one of which
 * may be the empty set. So if we join an empty set and a subtree, we should
 * just get the subtree (suppress degree two nodes).
 * *)
let joinOpt a b = 
  match (a, b) with
    | (Some x, Some y) -> Some (join x y)
    | (Some x, None) -> Some x
    | (None, Some y) -> Some y
    | (None, None) -> None

(* addCCOpt: add a CC to the given subtree, which may be the empty set. If it
 * is, then the CC becomes a CT.
 * *)
let addCCOpt a i = 
  match a with
    | Some x -> Some ( CC(x, i) )
    | None -> Some (CT i) (* the CC becomes a CT *)

let getCCLabel = function
  | Node(a, b, i) -> assert(false)
  | CC(a, i) -> i
  | CT(i) -> assert(false)
  | Leaf(i) -> assert(false)

let rec toString = function
  | Node(a, b, i) -> 
      "("^( toString a )^","^( toString b )^")"
      (*^"|"^( string_of_int i )^")"*)
  | CC(a, i) -> "["^( toString a )^"|"^( string_of_int i )^"]"
  | CT(i) -> "T"^( string_of_int i )
  | Leaf(i) -> "L"^( string_of_int i )

let print a = print_endline ( toString a )

let rec nLeaves = function
  | Node(a, b, i) -> ( nLeaves a ) + ( nLeaves b )
  | CC(a, i) -> nLeaves a
  | CT(i) -> 1
  | Leaf(i) -> 1

let isLeaf = function
  | Node(a, b, i) -> false
  | CC(a, i) -> false
  | CT(i) -> false
  | Leaf(i) -> true

let isTerminal = function
  | Node(a, b, i) -> false
  | CC(a, i) -> false
  | CT(i) -> true
  | Leaf(i) -> true

let isCC = function
  | Node(a, b, i) -> false
  | CC(a, i) -> true
  | CT(i) -> false
  | Leaf(i) -> false

let getCCInfo = function
  | Node(a, b, i) -> failwith "getCCInfo: not a CC"
  | CC(a, i) -> (a, i) 
  | CT(i) -> failwith "getCCInfo: not a CC"
  | Leaf(i) -> failwith "getCCInfo: not a CC"

(* getTerminalSet: make a set of terminal numbers *)
let rec toTerminalSet = function
  | Node(a, b, i) -> 
      Rsplit.IntSet.union (toTerminalSet a) (toTerminalSet b)
  | CC(a, i) -> Rsplit.IntSet.add i (toTerminalSet a)
  | CT(i) -> Rsplit.IntSet.singleton i
  | Leaf(i) -> Rsplit.IntSet.singleton i
  
(* getTerminalNumbers: make a list of terminal numbers *)
let getTerminalNumbers g = 
  Rsplit.IntSet.elements (toTerminalSet g)

(* *** testing if we have a triple or an extendo on top *)
let makePairs b singles = List.map ( fun c -> (b, c) ) singles
let makeTriples a pairs = List.map ( fun (b, c) -> (a, b, c) ) pairs
let addTerminal x (singles, pairs, triples) = 
  ( [x], makePairs x singles, ( makeTriples x pairs ) @ triples )
let triplesOnly (s, p, t) = t

(* topTriples: 
 * a triple is a chain of three terminals coming from the root. this function
 * looks for triples which sit right at the surface. remember that we only need
 * to look for triples which end in terminals because we assume that we have
 * contracted all isomorphic subtrees.
# toString h1;;
- : string = "(L0,(L1,(L2,(L3,L4))))"
# topTriples h1;;
- : (int * int * int) list = [(0, 1, 2)]
 *)
let topTriples ghost = 
  let rec aux depth ghost = 
    if depth >= 3 then ( [], [], [] )
    else (
      match ghost with
      | Node(a, b, i) -> (
        (* below: <> 0 so that we don't contract the root into a CC *)
          let isTerminalWithoutRoot c = isTerminal c && not (containsRoot c) in
          match isTerminalWithoutRoot a, isTerminalWithoutRoot b with
            | (true, true) -> ([getIndex a; getIndex b], [], [])
            | (false, true) -> addTerminal (getIndex b) (aux (depth+1) a)
            | (true, false) -> addTerminal (getIndex a) (aux (depth+1) b)
            | (false, false) -> 
      (* if we have two subtrees which aren't contracted already, then not a
       * chain, and if we don't have a surface triple now we won't get one by
       * heading deeper. *)
                ([], [], [])
        )
      (* chains can't traverse contracted chains, and if we don't have a surface
       * triple now we won't get one by heading deeper. *)
      | CC(a, i) -> ( [], [], [] ) 
      | CT(i) -> ( [], [], [] )
      | Leaf(i) -> ( [], [], [] )
  )
  in
  triplesOnly ( aux 0 ghost )

let rec isTopExtendo = function
  | Node(a, b, i) -> (
    if isTerminal a && isCC b then true
    else if isCC a && isTerminal b then true
    else false
  )
  | CC(a, i) -> false
  | CT(i) -> false
  | Leaf(i) -> false

(* findTopExtendo: if it is on the top, return Some ( pendant edge, CC ) *)
let rec findTopExtendo = function
  | Node(a, b, i) -> (
    if isTerminal a && isCC b then Some ( a, b )
    else if isCC a && isTerminal b then Some ( b, a )
    else None
  )
  | CC(a, i) -> None
  | CT(i) -> None
  | Leaf(i) -> None

(* *** contracting things *)

(* contractCherry: replace the cherry with indices index1 and index2 in ghost ghost. note
 * that these terminal nodes may be either leaves or CTs. index1 and index2 must be in the
 * correct order. *)
let contractCherry ghost index1 index2 = 
  assert ( index1 < index2 ); (* make sure i and j are in correct order *)
  let contracted = ref false in
  let rec aux = function
    | Node(a, b, i) -> (
      match ( isTerminal a, isTerminal b ) with
      | ( true, true ) -> 
          if index1 = getIndex a && index2 = getIndex b then (
            (* we are in the money. replace the cherry with a CT *)
            contracted := true;
            CT ( min index1 index2 )
          )
          else
            (* found a cherry, but not the desired one *)
            Node (a, b, i)
      | ( false, true ) -> Node ( aux a, b, i )
      | ( true, false ) -> Node ( a, aux b, i )
      | ( false, false ) -> Node ( aux a, aux b, i )
      )
    | CC(a, i) -> CC( aux a, i )
    | CT(i) -> CT(i)
    | Leaf(i) -> Leaf(i)
  in
  let newGhost = aux ghost in
  (!contracted, newGhost )

(* pullPendantEdge: if the tree is a pendant edge and a subtree, then return the
 * ct/leaf and the subtree, in that order. Usual order if a cherry. *)
let pullPendantEdge = function 
  | Node(a, b, i) -> 
      if isTerminal a then
        if isTerminal b then
          (* cherry *)
          Some (a, b)
        else
          (* a is a leaf, but b is not *)
          Some ( a, b )
      else
        if isTerminal b then
          (* b is a leaf, but a is not *)
          Some ( b, a )
        else
          None
  | CC(a, i) -> None
  | CT(i) -> None
  | Leaf(i) -> None

(* checkChain: check if the tree has a given chain of leaf/ct's in order
 * descending from the root. return None if no, return Some ( subtree below ) if
 * yes. *)
let rec checkChain ghost = function
  | top :: rest -> (
      match pullPendantEdge ghost with 
      | Some ( a, b ) ->
          if isTerminal b && rest = [] then 
            (* we have a cherry and there aren't any other things to match *)
            if getIndex a = top then Some b
            else if getIndex b = top then Some a
            else None
          else if top = getIndex a then (
            (* we match, and it's not a cherry *)
              checkChain b rest 
            )
          else (
            None
            (* we don't match, but might below 
            checkChain b ( top::rest ) *)
          )
      | None -> None (* no pendant edges *)
      )
    | [] -> Some ghost
      
let tripleMin (x, y, z) = min x ( min y z )

(* contractChain: contract the xyz-chain (in order coming from the root) if it
 * exists in the ghost. we don't contract if the tree below the chain should
 * contain the root but doesn't. for details on that, see the comments for
 * coalesceContract. 
 * *)
let contractChain ghost (x, y, z) shouldContainRoot = 
  let contracted = ref false
  in
  let rec aux t = 
    match t with
    | Node(a, b, i) -> (
      match checkChain t [x; y; z] with
      | Some subtree -> 
          if shouldContainRoot = containsRoot subtree then (
            contracted := true;
            CC (subtree, tripleMin (x, y, z))
          )
  (* if the top is an xyz-chain but the subtree below doesn't have the same
   * contain-root status as desired, then we don't need to recur any more as
   * that xyz-chain won't appear again *)
          else subtree
      | None -> Node ( aux a, aux b, i)
    )
    | CC(a, i) -> CC( aux a, i )
    | CT(i) -> CT(i)
    | Leaf(i) -> Leaf(i)
  in
  (* whoa! don't "fix" below. have to write it like this so contracted gets set *)
  let newGhost = aux ghost in
  (!contracted, newGhost )

(* certainContractChain: use when we are sure we can contract *)
let certainContractChain ghost (x, y, z) shouldContainRoot = 
  match contractChain ghost (x, y, z) shouldContainRoot with
  | true, newGhost -> newGhost
  | false, oldGhost -> failwith "certainContractChain: couldn't contract!"

(* contractExtendo: contract the extendo with CC label ccLabel and leaf/CT index
 * leafCTInd. *)
let contractExtendo ghost ccLabel ctLeafInd = 
  let contracted = ref false
  in
  let rec aux = function
    | Node(a, b, i) -> (
      if isTerminal a && isCC b && 
           getIndex a = ctLeafInd && getCCLabel b = ccLabel then (
        contracted := true;
        CC ( aux ( fst ( getCCInfo b ) ), min ccLabel ctLeafInd )
      )
     else if isTerminal b && isCC a && 
           getIndex b = ctLeafInd && getCCLabel a = ccLabel then (
        contracted := true;
        CC ( aux ( fst ( getCCInfo a ) ), min ccLabel ctLeafInd )
      )
     else Node ( aux a, aux b, i )
    )
    | CC(a, i) -> CC( aux a, i )
    | CT(i) -> CT(i)
    | Leaf(i) -> Leaf(i)
  in
  (* whoa! don't "fix" below. have to write it like this so contracted gets set *)
  let newGhost = aux ghost in
  (!contracted, newGhost )

(* certainContractExtendo: use when we are sure we can contract *)
let certainContractExtendo ghost ccLabel ctLeafInd = 
  match contractExtendo ghost ccLabel ctLeafInd with
  | true, newGhost -> newGhost
  | false, oldGhost -> failwith "certainContractExtendo: couldn't contract!"

(* *** general functions *)
let rec comb n = 
  if n = 0 then Leaf 0
  else join ( comb (n-1) ) ( Leaf n )

(* yule: makes a Yule tree on n taxa *)
let yule n = 
  let rec aux start len = 
    if len = 1 then Leaf start
    else
      let splitLen = 1 + ( Random.int (len-1) ) in
      Node ( aux start splitLen, 
             aux (start + splitLen) ( len - splitLen ),
             start)
  in
  aux 0 n

(* inducedGhost: isIn is a function which takes index numbers and returns true
 * if that taxon should be in the ghost *)
let rec inducedGhost isIn = function
  | Node(a, b, i) -> joinOpt (inducedGhost isIn a) (inducedGhost isIn b) 
  | CC(a, i) -> 
      let below = inducedGhost isIn a in
      if isIn i then addCCOpt below i
      else below
  | CT(i) -> if isIn i then Some (CT i) else None
  | Leaf(i) -> if isIn i then Some (Leaf i) else None


(* *** alternative representations of ghosts *** *)

(* taxaAboveAndBelow: return bool arrs (aboveTaxa, belowTaxa) giving the taxa
 * above and below a given edge. n is the (max) number of taxa, not the maximum
 * index.
 * *)
let taxaAboveAndBelow n ghost cutEdge = 
  let aboveTaxa = Array.make n false
  and belowTaxa = Array.make n false
  in
  let rec taxaBelow = function
      | Node(a, b, i) -> taxaBelow a; taxaBelow b
      | CC(a, i) -> belowTaxa.(i) <- true; taxaBelow a
      | CT(i) -> belowTaxa.(i) <- true
      | Leaf(i) -> belowTaxa.(i) <- true
  in
  let rec taxaAbove edge = 
    if edge = cutEdge then taxaBelow edge
    else (
      match edge with
      | Node(a, b, i) -> taxaAbove a; taxaAbove b
      | CC(a, i) -> aboveTaxa.(i) <- true; taxaAbove a
      | CT(i) -> aboveTaxa.(i) <- true
      | Leaf(i) -> aboveTaxa.(i) <- true
    )
  in
  taxaAbove ghost;
  (aboveTaxa, belowTaxa)

let rec toEdgeList ghost = 
  match ghost with 
  | Node(a, b, i) -> ghost::((toEdgeList a)@(toEdgeList b))
  | CC(a, i) -> ghost::(toEdgeList a)
  | CT(i) -> [ghost]
  | Leaf(i) -> [ghost]

(* toEdgeRsplitList: make a list of rSplits associated with a ghost. designed
 * such that the first element of the rSplit list is the root split. *)
let toEdgeRsplitList allTaxaSet tree = 
  let rec aux = function
    | Node(a, b, i) -> 
        let aRSplits = aux a
        and bRSplits = aux b in
        assert(aRSplits <> [] && bRSplits <> []);
        (Rsplit.union (List.hd aRSplits) (List.hd bRSplits))
        ::(aRSplits @ bRSplits)
    | CC(a, i) -> 
      let aRSplits = aux a in
      assert(aRSplits <> []);
      (Rsplit.add i (List.hd aRSplits))::aRSplits
  | CT(i) -> [Rsplit.singleton i]
  | Leaf(i) -> [Rsplit.singleton i]
  in
  Rsplit.rerootList allTaxaSet (aux tree)

(* getConvexInfo: 
 * raise PartitionIncompatible if the ghost is not compatible with the
 * partition, otherwise return the elements of the partition which could be the
 * root's partition, along with those which have been used and can't be root
 * part'n.
 *)
let getConvexInfo partition ghost = 
  (* checkRootUsed: 
   * if the root labels intersect the used labels then raise incompatible *)
  let checkDisjoint root used = 
    if not (IS.is_empty (IS.inter root used)) then
      raise PartitionIncompatible
  in
  let rec aux = function
    | Node(a, b, i) -> 
        let aRoot, aUsed = aux a 
        and bRoot, bUsed = aux b in
        (* below: make sure the useds and the roots don't overlap *)
        checkDisjoint (IS.union aRoot aUsed) bUsed;
        checkDisjoint (IS.union bRoot bUsed) aUsed;
        let bothUsed = IS.union aUsed bUsed in
        let newRoot = IS.inter aRoot bRoot in
        let s = IS.cardinal newRoot in
        if s = 0 then
          (* no intersection, do the root could be any of the below roots *)
          IS.union aRoot bRoot, bothUsed
        else if s = 1 then
          (* the root state is determined and everything else becomes used *)
          newRoot,
          IS.union bothUsed (
            IS.diff (IS.union aRoot bRoot) newRoot)
        else
            raise PartitionIncompatible
    | CC(a, i) -> 
      let root, used = aux a
      and singleI = IS.singleton partition.(i) in
      checkDisjoint singleI used;
(* below diff: if the singleI is already in root, then we don't want it in used *)
      singleI, IS.diff (IS.union root used) singleI
  | CT(i) -> IS.singleton partition.(i), IS.empty
  | Leaf(i) -> IS.singleton partition.(i), IS.empty
  in
  try 
    aux ghost
  with
    | Invalid_argument x -> failwith ("getConvexInfo: partition too small, "^x)

(* *** testing *** *)


(* toCoalSeqAndForest: gives the coalescent sequence and the forest that
 * gives the supplied ghost. Note that this gives the coals in the proper order:
 * so that the lowest ones are the first in the list. Note that this function 
 * needs to have taxon set 0,... n-1. *)
let toCoalSeqAndForest ghost = 
  let forest = Array.make ( nLeaves ghost ) None in
  let rec aux = function
    | Node(a, b, i) -> 
        (Base.sortPair (getIndex a, getIndex b)) :: ( ( aux a ) @ ( aux b ) )
    | CC(a, i) -> forest.(i) <- Some ( CC(a, i) ); []
    | CT(i) -> forest.(i) <- Some (CT i); []
    | Leaf(i) -> forest.(i) <- Some (Leaf i); []
  in
  try
    let seq = aux ghost in
    ( List.rev seq, forest )
  with
    | Invalid_argument x -> failwith (x^" in toCoalSeqAndForest")
    
(* toCoalSeq:
 * in contrast to the previous function, this makes no assumptions about the
 * taxon set, which is why there is some duplicate code here. *)
let toCoalSeq ghost = 
  let rec aux = function
    | Node(a, b, i) -> 
        (Base.sortPair (getIndex a, getIndex b))::((aux a) @ (aux b))
    | CC(a, i) -> []
    | CT(i) -> []
    | Leaf(i) -> []
  in
  List.rev (aux ghost)

let yuleCoalSeq n = 
  toCoalSeq (yule n)

let rec ofObtree = function
  | Obtree.Node(a, b, i) -> join (ofObtree a) (ofObtree b) 
  | Obtree.Leaf(i) -> Leaf (i) 

(* ofNewick: not a proper parsing function *)
let ofNewick s = ofObtree (Obtree.ofNewick s)



let a = join (Leaf 0) (Leaf 1)
let b = CC( Leaf 2, 3)
let c = join a b
let d = join c (join (Leaf 4) (CT 5))
let e = yule 9
let f = comb 7
let g = join (join (CC( CC(comb 2, 3), 4)) (Leaf 5)) (CC(Leaf 6, 7))
let gp = join (Leaf 8) (CC(Leaf 9,10))
let h1 = ofNewick "(0,(1,(2,(3,4))))"
let h2 = ofNewick "(4,(1,(2,(3,0))))"
let x i = (i=0) ||(i=1) ||(i=3)
