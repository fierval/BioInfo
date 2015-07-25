#load @"..\Lesson 2\environment.fsx"
#load "DataStructs.fsx"

open System.Linq
open System
open DataStructs

// array of amino acid weights
let weights = (aminoAcidOneLetterIntegerMass |> Seq.map (fun kvp -> kvp.Value)).Distinct() |> Seq.toArray

/// Count then number of peptides of mass m
let countNumPeptides m =
    if m < weights.[0] then 0L
    else
        let allCounts : int64 [] = Array.zeroCreate (max m weights.[weights.Length - 1] + 1)
        
        // initialize dynamic programming store
        Array.ForEach(weights, fun w -> allCounts.[w] <- 1L)
        let dynamicProgStep n =
            let filtered = weights |> Array.filter (fun w -> (n - w) >= weights.[0]) 
            if filtered.Length > 0 then
                int64 (filtered
                    |> Array.map (fun w -> allCounts.[n - w])
                    |> Array.sum)
            else
                0L

        // equivalent to a simple loop, but we are using tail recursion
        let rec fillDynArray n =
            if n > m then allCounts.[m]
            else
                allCounts.[n] <- allCounts.[n] + dynamicProgStep n
                fillDynArray (n + 1)

        fillDynArray weights.[0]               