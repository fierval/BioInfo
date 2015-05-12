open System
open System.Linq
open System.IO
open System.Diagnostics

#load "mostProbableKMer.fsx"
#load "regulatoryMotifs.fsx"

#load "randomizedMotifSearch.fsx"
#load "..\packages\MathNet.Numerics.FSharp.3.6.0\MathNet.Numerics.fsx"

open MathNet.Numerics.Random

open RegulatoryMotifs
open MostProbableKMer

let sw = Stopwatch()

let rndSeed = RandomSeed.Robust()
let rnd = Random.mersenneTwisterSeed rndSeed

let init2D (arr : 'a [][]) =
    let rows = arr.Length
    let cols = arr.[0].Length

    Array2D.init rows cols (fun i j -> arr.[i].[j])

let createProfile (motifs : int[,]) =
    let k = Array2D.length2 motifs
    let t = Array2D.length1 motifs
    let completeSet = [0..3]
    let tmp = 
        [|0..Array2D.length2 motifs - 1|] 
        |> Array.map 
            (fun col -> motifs.[0..,col].GroupBy(fun c -> c).OrderBy(fun gr -> gr.Key).Select(fun gr -> gr.Key, float(gr.Count() + 1) / float(t + 4)).ToArray())
            
    let pseudoCount = 1. / (float t + 4.)
    let profile =
        [|for row in tmp do
            let keys = row |> Array.map(fun (e, c) -> e)
            let missing = completeSet.Except(keys).ToArray()
            if missing.Length = 0 then yield row
            else
                yield missing |> Array.map(fun k -> (k, pseudoCount)) |> Array.append row |> Array.sortBy (fun (e, k) -> e)
            
        |]      
    Array2D.init 4 k (fun i j -> snd profile.[j].[i])
    
// search for motifs by making random selections
// iters - how many iterations before stopping
let randomizedMotifs (dna : string []) k iters =
    let t = dna.Length

    // core of the algorithm:
    // select random motifs from each dna string
    // score it and create its profile for the next selection
    let singleRun () =
        let len = dna.[0].Length - k

        let firstMotifs = [|1..t|] |> Array.map (fun i -> rnd.Next(0, len)) |> Array.mapi (fun i p -> dna.[i].Substring(p, k) |> toInts)
        let bestMotifs = Array2D.init t k (fun i j -> firstMotifs.[i].[j])
        let bestScore = score bestMotifs
        let profile = createProfile bestMotifs

        let rec findBest bestScore profile =
            let motifs = dna |> Array.map (fun s -> findMostProbable s profile |> toInts) |> init2D
            let curScore = score motifs
            if curScore = bestScore then
                curScore, motifs
            else
                findBest curScore (createProfile motifs)

        findBest bestScore profile

    // repeat the above many times. Best of all - in parallel
    // run everything in parallel: 8 times faster on Core i7-4820K 3.7 GHz
    let scoreMotifs = [1..iters].AsParallel().Select(fun i -> singleRun())
    let best, motif = scoreMotifs |> Seq.minBy (fun (sc, e) -> sc)

    [|0..t-1|] |> Array.map (fun i -> motif.[i, 0..] |> toStr)

    

let randomizedMotifsFile file = 
    let lines = File.ReadAllLines(file)
    let k = int (lines.[0].Trim().Split(' ').First())
    sw.Reset()
    sw.Start()
    let motifs = randomizedMotifs lines.[1..] k 1000
    sw.Stop()
    printfn "Elapsed: %s" (sw.Elapsed.ToString())

    File.WriteAllLines(@"c:\temp\sol6.txt", motifs)