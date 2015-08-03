#load @"..\Lesson 2\environment.fsx"
#load "DataStructs.fsx"

open System.Linq
open System
open DataStructs

// array of amino acid weights
let weights = (aminoAcidOneLetterIntegerMassTrunc |> Seq.map (fun kvp -> kvp.Value)) |> Seq.toArray

/// Count the number of peptides of mass m
let countNumPeptides m =
    let allCounts : int64 [] = Array.zeroCreate (max m weights.[weights.Length - 1] + 1)
        
    // initialize dynamic programming store
    Array.ForEach(weights, fun w -> allCounts.[w] <- 1L)

    // equivalent to a simple loop, but we are using tail recursion
    let rec fillDynArray n =
        if n > m then allCounts.[m]
        else
            // dynamic_count(N) = Sum(dynamic_count((N - weights[0]), dynamic_count(N - weights[1]),.., dynamic_count(N - weights.Last())
            let step = weights |> Array.map (fun w -> if n - w < 0 then 0L else allCounts.[n - w])|> Array.sum

            allCounts.[n] <- allCounts.[n] + step
            fillDynArray (n + 1)

    fillDynArray weights.[0]