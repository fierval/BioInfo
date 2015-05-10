open System
open System.Linq
open System.IO

#load "mostProbableKMer.fsx"
#load "regulatoryMotifs.fsx"

open RegulatoryMotifs
open MostProbableKMer

let rnd = Random(int DateTime.Now.Ticks)
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
    

let motifs_str = [|"CTTGT"; "GCTCG"; "TAGAT"; "ACTTA"; "CGCTA"; "ACGAG"|]
let motifs = motifs_str |> Array.map (fun m -> toInts m) |> init2D

// search for motifs by making random selections
// iters - how many iterations before stopping
let randomizedMotifs (dna : string []) k iters =
    let t = dna.Length
    let len = dna.[0].Length - k

    let firstMotifs = [|1..t|] |> Array.map (fun i -> rnd.Next(0, len)) |> Array.mapi (fun i p -> dna.[i].Substring(p, k) |> toInts)
    let bestMotifs = Array2D.init t k (fun i j -> firstMotifs.[i].[j])
    let bestScore = score bestMotifs
    let profile = createProfile bestMotifs

    let rec findBest motifs bestMotifs bestScore profile iters =
      if iters = 0 then bestMotifs 
      else
        let motifs = dna |> Array.map (fun s -> findMostProbable s profile |> toInts) |> init2D
        let curScore = score motifs

        findBest motifs (if curScore < bestScore then motifs else bestMotifs) (if curScore < bestScore then curScore else bestScore) (createProfile motifs) (iters - 1)

    let best = findBest bestMotifs bestMotifs bestScore profile (iters - 1)

    [|0..t-1|] |> Array.map (fun i -> best.[i, 0..] |> toStr)

let randomizedMotifsFile file = 
    let lines = File.ReadAllLines(file)
    let k = int (lines.[0].Trim().Split(' ').First())
    let motifs = randomizedMotifs lines.[1..] k 1000
    File.WriteAllLines(@"c:\temp\sol6.txt", motifs)