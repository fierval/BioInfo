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

let stackV (a1: 'a[,]) (a2: 'a[,]) =
    let a1l1,a1l2,a2l1,a2l2 = (Array2D.length1 a1),(Array2D.length2 a1),(Array2D.length1 a2),(Array2D.length2 a2)
    if a1l2 <> a2l2 then failwith "arrays have different column sizes"
    let result = Array2D.zeroCreate (a1l1 + a2l1) a1l2
    Array2D.blit a1 0 0 result 0 0 a1l1 a1l2
    Array2D.blit a2 0 0 result a1l1 0 a2l1 a2l2
    result

let sw = Stopwatch()

let createProfile (motifs : int[,]) =
    let k = Array2D.length2 motifs
    let t = Array2D.length1 motifs + 4

    let appendLaplace () =
        let As = Array.zeroCreate k
        let rest = [|yield As; for i = 1 to 3 do yield As |> Array.map (fun a -> a + i) |] |> array2D
        stackV motifs rest
    
    let m = appendLaplace()
    let profile = 
        [|0..k - 1|] 
        |> Array.map 
            (fun col -> m.[0..,col].GroupBy(fun c -> c).OrderBy(fun gr -> gr.Key).Select(fun gr -> float(gr.Count()) / float t).ToArray())
            
    Array2D.init 4 k (fun i j -> profile.[j].[i])
    
// search for motifs by making random selections
// iters - how many iterations before stopping
let randomizedMotifs (dna : string []) k iters =
    let t = dna.Length
    let rndSeed = RandomSeed.Robust()
    let rnd = Random.mersenneTwisterSeed rndSeed

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
            let motifs = dna |> Array.map (fun s -> findMostProbable s profile |> toInts) |> array2D
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