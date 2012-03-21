let () = ()

let jcCorr subsFract =  -. 0.75 *. (log ( 1. -. (4./.3.) *. subsFract ))

let nObservableMuts rng branchLength seqLength = 
  Gsl_randist.binomial rng 0.75 (
    Gsl_randist.poisson rng (branchLength *. (float_of_int seqLength)))

let observableMutsPerSite rng branchLength seqLength = 
  assert(seqLength <> 0);
  Base.int_div (nObservableMuts rng branchLength seqLength) seqLength

let findPerturbedDM rng seqLength beTree = 
  Mat.AAR.map jcCorr ( 
    Betree.toDistMat (
      Betree.mapEdgeLengths (
        fun branchLength ->
          observableMutsPerSite rng branchLength seqLength
      ) beTree
    )
  )


