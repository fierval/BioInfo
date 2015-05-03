
open System
open System.Linq
open System.Collections.Generic
open System.IO

#load "environment.fsx"
#load "mostProbableKMer.fsx"
#load @"..\Lesson 1\countKmers.fsx"

open MostProbableKMer
open CountKmers

let rev_alphabet = alphabet |> Seq.map(fun kvp -> kvp.Key.ToString()) |> Seq.toArray

let toInts (motif : string) =
    motif.ToCharArray() |> Array.map (fun c -> alphabet.[c])
    
let toStr (motif : int []) =
    motif |> Array.fold(fun state i -> state + rev_alphabet.[i]) String.Empty

/// <summary>
/// Create profile from a given profile and an array of 
/// </summary>
/// <param name="seed"></param>
/// <param name="dna"></param>
let augmentProfile (seed : float [,]) (motif : string) t =
    let k = motif.Length
    let motifs = motif |> toInts
    if t = 0. then
        Array2D.init 4 k (fun i j -> if i = motifs.[j] then 1. else 0.)
    else
        let scores = seed |> Array2D.map (fun s -> int (s * t))
        let newScores = Array2D.init 4 k (fun i j -> if i = motifs.[j] then scores.[i,j] + 1 else scores.[i,j])
        newScores |> Array2D.map (fun e -> float e / (t + 1.))

let colScore (col : int []) =
    col.Length - col.GroupBy(fun c -> c).Select(fun gr -> gr.Count()).Max()

let score (motifs : int [,]) =
    [0..Array2D.length2 motifs - 1] 
    |> List.map (fun col -> motifs.[0..,col] |> colScore) 
    |> List.sum

let applyLaplace (profile : float[,]) t =
    profile |> Array2D.map (fun s -> (s * t + 1.) / (t + 4.))

/// <summary>
/// Finding regulatory motifs by greedy search.
/// </summary>
/// <param name="dna">Array of genes</param>
/// <param name="k">length of the polymer</param>
let greedyMotifSearch (dna : string []) k pseudocounts =
    let t = dna.Length

    
    let firstMotifs = [|0..t-1|] |> Array.map (fun i -> dna.[i].Substring(0, k)) |> Array.map(fun m -> toInts m)
    let mutable bestMotifs = Array2D.init t k (fun i j -> firstMotifs.[i].[j])
    let mutable bestScore = score bestMotifs

    // Use the first line in dna as a seed
    for km in kMers dna.[0] k do
        let kmer = km.Key
        let seed = Array2D.zeroCreate 0 0
        let mutable profile = augmentProfile seed kmer 0.
        let bestMotifsStr = [kmer].ToList()
        // build up the profile and the current selection
        // of motifs based on the running profile
        for i in [1..t-1] do
            let pseudoProfile = if pseudocounts then applyLaplace profile (float i) else profile 
            let motif = findMostProbable dna.[i] pseudoProfile
            bestMotifsStr.Add(motif)
            profile <- augmentProfile profile motif (float i)

        
        // compare with the running best score and set the current one as "best"
        // if it is smaller
        let curMotfisFromList = bestMotifsStr.Select(fun s -> toInts s).ToArray()
        let curMotifs = Array2D.init t k (fun i j -> curMotfisFromList.[i].[j])
        let curScore = score curMotifs
        if curScore < bestScore then
            bestScore <- curScore
            bestMotifs <- curMotifs
    
    let res = bestMotifs
    [|0..Array2D.length1 bestMotifs - 1|] |> Array.map(fun row -> res.[row, 0..] |> toStr)

let greedyMotifSearchFile fileName pseudocounts =
    let lines = File.ReadAllLines(fileName)
    let k = int (lines.[0].Split(' ').[0])
    let dna = lines.[1..]
    File.WriteAllLines(@"c:\temp\regMotifs.txt", greedyMotifSearch dna k pseudocounts)