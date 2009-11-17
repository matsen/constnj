

class ['a] tensor arg = 

  let dimArrToBlockSizeArr dimArr = 
    let blockSizeArr = Array.make (Array.length dimArr) 1 in
    for i=0 to (Array.length dimArr)-1 do
      for j=i+1 to (Array.length dimArr)-1 do
        blockSizeArr.(i) <- blockSizeArr.(i) * dimArr.(j)
      done;
    done;
    blockSizeArr
  in

  let nDims, dimArr, blockSizeArr, tArr = 
    match arg with
      | `Make (dimArr, (elt:'a)) -> 
          Array.length dimArr,
          dimArr,
          dimArrToBlockSizeArr dimArr,
          Array.make (Array.fold_left ( * ) 1 dimArr) elt

  in

object (self)

  method private checkMIndexDim mInd = 
    if Array.length mInd <> nDims then
      invalid_arg "mIndex of wrong dimension"

  method isLegalMIndex mInd = 
    self#checkMIndexDim mInd;
    try
      for i=0 to nDims-1 do
        if mInd.(i) >= dimArr.(i) then raise Exit
      done;
      true
    with | Exit -> false

  method private checkMIndex mInd = 
    self#checkMIndexDim mInd;
    if not (self#isLegalMIndex mInd) then
      invalid_arg "mIndex out of bounds"

  (*
   # let d = dimArrToBlockSizeArr [|2;3;2|];;
   val d : int array = [|6; 2; 1|]
   # flattenIndex d [|1;1;1|];;              
   - : int = 9
   *)
  method flattenMIndex mInd = 
    self#checkMIndex mInd;
    let flatInd = ref 0 in
    for i=0 to (Array.length blockSizeArr) - 1 do
      flatInd := !flatInd + blockSizeArr.(i) * mInd.(i)
    done;
    !flatInd
      
  method unflattenIndex ind = 
    if 0 > ind || ind >= Array.length tArr then
      invalid_arg "unflattenIndex: index out of bounds!";
    let mInd = Array.make nDims 0 in
    let indRemaining = ref ind in
    for i=0 to nDims-1 do
      while blockSizeArr.(i) <= !indRemaining do
        mInd.(i) <- mInd.(i) + 1;
        indRemaining := !indRemaining - blockSizeArr.(i)
      done;
    done;
    assert(!indRemaining = 0);
    for i=0 to nDims-1 do assert(mInd.(i) < dimArr.(i)) done;
    mInd

  method iter f = Array.iter (fun x -> f x) tArr
  method iteri f =
    Array.iteri (fun i x -> f (self#unflattenIndex i) x) tArr

  method getNDims = nDims 
  method getDimArr = dimArr 
  method get mInd = tArr.(self#flattenMIndex mInd) 
  method set mInd x = tArr.(self#flattenMIndex mInd) <- x 

  method getTensorArr = tArr
  method print eltToStr = 
    self#iteri (
      fun mInd x ->
        Printf.printf "%s: %s\n" 
          (Base.intArrToString mInd) (eltToStr x))

  method testUnflat i = 
    let mInd = self#unflattenIndex i in 
    Printf.printf "%d:  " i;
    Base.printIntArr mInd;
    assert(i = self#flattenMIndex mInd);

  method multiTest = 
    for i=0 to (Array.length tArr)-1 do
      self#testUnflat i
    done

end

let iter2 f t1 t2 = 
  if t1#getDimArr <> t2#getDimArr then
    invalid_arg "Tensor.iter2: dimensions don't match!";
  for i=0 to (Array.length t1#getTensorArr) -1 do
    f t1#getTensorArr.(i) t2#getTensorArr.(i) 
  done

let init dimArr f = 
  let nDims = Array.length dimArr in
  let t = new tensor(`Make(dimArr, f (Array.make nDims 0))) in
  t#iteri (fun mInd x -> t#set mInd (f mInd));
  t

let map f t = init t#getDimArr (fun mInd -> f (t#get mInd))

let mapi f t = init t#getDimArr (fun mInd -> f mInd (t#get mInd))

let map2 f t1 t2 = 
  if t1#getDimArr <> t2#getDimArr then
    invalid_arg "Tensor.map2: dimensions don't match!";
  init t1#getDimArr (
    fun mInd -> f (t1#get mInd) (t2#get mInd))

let fold_left f init t = Array.fold_left f init t#getTensorArr
