#load @"..\Chapter 3 Molecular Clocks\environment.fsx"
#load "DataStructs.fsx"

open System.Linq
open System.Collections.Generic
open System
open System.IO
open DataStructs

// array of amino acid weights
let weights = (aminoAcidOneLetterIntegerMassTrunc |> Seq.map (fun kvp -> kvp.Value)) |> Seq.toArray

let cyclospec (peptide : int seq) =
    let len = peptide.Count()
    let pepArr = peptide |> Seq.toArray

    let mutable parsum = 0

    (seq {
        yield 0
        yield pepArr |> Array.sum
        for i = 0 to len - 2 do
            yield! [0..len-1] 
                |> Seq.map 
                    (fun j ->  
                        //F# 4.0 Feature!
                        parsum <- 0
                        for ind = j to j + i do
                            parsum <- parsum + pepArr.[if ind >= len then ind - len else ind]
                        parsum    
                        )
    }) |> Seq.sort 
        

let cyclospectrum (peptide : string) =
    let masses = 
        peptide.ToCharArray() 
        |> Array.map (fun c -> aminoAcidOneLetterIntegerMass.[c])

    cyclospec masses    

let solveCyc (peptide : string) =
    let solution = cyclospectrum peptide
    let txt = solution |> Seq.fold (fun state e -> if String.IsNullOrEmpty state then e.ToString() else state + " " + e.ToString()) String.Empty
    File.WriteAllText(@"c:\temp\ant3.txt", txt)

let generatePeptide (len : int) =
    let rnd = Random(int DateTime.UtcNow.Ticks)

    seq {
        for i in [1..len] -> weights.[rnd.Next(weights.Length)]
    } |> Seq.sort |> Seq.toArray