(*
let gf1 = GhostForestFuns.ofNewick "(0,(((((1,2),3),4),5),6))";;
let gf2 = GhostForestFuns.ofNewick "(0,(((((6,5),4),3),2),1))";;
let () = gf1#printGhosts;;
let () = gf2#printGhosts;;
let xx = verbosePartitionIsAP [|0;1;1;2;2;3;3|] gf1 gf2;;

let gf1 = GhostForestFuns.ofNewick "(((((0,1),2),3),4),5)";;
let gf2 = GhostForestFuns.ofNewick "(((((5,4),3),2),1),0)";;


let gf1 = GhostForestFuns.gfOfStringArr [| "(0,4)"; "(1,3)"; "2"; ""; "" |];;
let gf2 = GhostForestFuns.gfOfStringArr [| "0"; "(1,2)"; ""; "(3,4)"; "" |];;

gf1#print;;
gf2#print;;

let before = partitionIsAP [|0;0;0;1;0|] gf1 gf2;;

let bx = (gf1#toEdgeRsplitList, gf2#toEdgeRsplitList);;

GhostForestFuns.coalesceContract gf1 gf2 0 1;;

gf1#print;;
gf2#print;;

let by = (gf1#toEdgeRsplitList, gf2#toEdgeRsplitList);;

let after = partitionIsAP [|0;0;0;1;0|] gf1 gf2;;
let partition = [|0;0;0;1;0|];;


let gf1 = GhostForestFuns.gfOfStringArr [| "((0,5),1)"; ""; "(2,6)"; "(3,4)"; "";  "";  ""; |];;
let gf2 = GhostForestFuns.gfOfStringArr [| "((0,5),((2,(3,4)),6))";  "1"; ""; ""; ""; ""; ""; |];;

let after = partitionIsAP [|0;0;0;0;0;0;1|] gf1 gf2;;


let beef1 = GhostForestFuns.gfOfStringArr [| "(0,3)"; "(1,5)"; "2"; ""; ""; ""; "" |]
let beef2 = GhostForestFuns.gfOfStringArr [| "(0,2)"; "1"; ""; "(3,5)"; ""; ""; "" |]

let gf1 = GhostForestFuns.gfOfStringArr [| "(0,3)"; "((1,5),2)"; ""; ""; ""; ""; "" |];;
let gf2 = GhostForestFuns.gfOfStringArr [| "(0,2)"; "1"; ""; "((3,5)"; ""; ""; "" |];;

let after = partitionIsAP [|0;0;0;1;1;0;1|] gf1 gf2;;

beef1#toRelevantEdgeRsplitList;;
gf1#toRelevantEdgeRsplitList;;
gf2#toEdgeRsplitList;;

let before1 = GhostForestFuns.gfOfStringArr [| "0"; "(1,5)"; "2"; "3"; "4"; ""; |];;
let x1 = GhostForestFuns.gfOfStringArr [| "(0,(1,5))"; ""; "2"; "3"; "4"; ""; |];;
let x2 = GhostForestFuns.gfOfStringArr [| "((0,4),2)"; "1"; ""; "3"; ""; "5"; |];;

let y1 = GhostForestFuns.gfOfStringArr [| "(((0,4),(6,7)),(3,(8,9)))"; "(1,5)"; "2"; ""; ""; ""; ""; ""; ""; "" |];;
let y2 = GhostForestFuns.gfOfStringArr [| "(0,((1,5),4))"; ""; "(2,(3,(8,9)))"; ""; ""; ""; "(6,7)"; ""; ""; "" |];;

let z1 = GhostForestFuns.gfOfStringArr [| "(((0,4),6),3)"; "1"; "2"; ""; ""; ""; ""; ""; ""; "" |];;
let z2 = GhostForestFuns.gfOfStringArr [| "(0,(1,4))"; ""; "(2,3)"; ""; ""; ""; "6"; ""; ""; "" |];;

let w1 = GhostForestFuns.gfOfStringArr [| "(((0,1),5),4)"; ""; "2"; "3"; ""; ""; "6"; "7"; "8"; "9" |];;
let w2 = GhostForestFuns.gfOfStringArr [| "((0,(4,5)),1)"; ""; "2"; "3"; ""; ""; "6"; "7"; "8"; "9" |];;

let testy = GhostForestFuns.gfOfStringArr [| "(0,1)"; "(2,3)"; "(4,5)"; ""; ""; ""; |];;

 *)

let w1 = GhostForestFuns.gfOfStringArr [| "(0,(((1,3),4),2))"; ""; ""; ""; ""; |]
let w2 = GhostForestFuns.gfOfStringArr [| "(0,((1,(2,4)),3))"; ""; ""; ""; ""; |]
