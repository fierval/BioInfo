#load @"..\Lesson 1\maxMutation.fsx"

open System.IO
open System.Linq
open System.Collections.Generic
open MaxMutation
open FuzzyMatch
open System

Environment.CurrentDirectory <- @"C:\Users\Boris\Downloads"

/// <summary>
/// generate all k-mers from the ititial seed s
/// </summary>
/// <param name="s"></param>
let generateKmers k =
    let s = String('A', k)
    let initial = kMersMutat s k k
    seq{yield s; yield! getMutations initial k} |> Seq.toArray
    
let findMedianBrute (dna : string []) k =
    let d (s : string) (pat : string) =
        [0..s.Length-pat.Length].AsParallel().Select(fun i -> hammingDist (s.Substring(i, pat.Length)) pat)
            .Min()
    
    let dist (dna : string []) pat =
        dna.AsParallel().Select(fun s -> d s pat).Aggregate(0, fun state dst -> state + dst)

    let kmers = generateKmers k

    let distances = kmers |> Array.map (fun pat -> dist dna pat) |> Seq.zip kmers
    let minTuple = distances.GroupBy(fun (km, dist) -> dist).OrderBy(fun gr -> gr.Key).First()
    fst (minTuple.Single())
        
let findMedianBruteFile fileName =
    let inp = File.ReadAllLines(fileName)
    let k = int (inp.[0].Trim())
    findMedianBrute inp.[1..] k
