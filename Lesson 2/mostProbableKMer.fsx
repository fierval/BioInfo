//#load @"..\packages\MathNet.Numerics.FSharp.Signed.3.6.0\MathNet.Numerics.fsx"

//open MathNet.Numerics.LinearAlgebra
#load "environment.fsx"

open System.Linq
open System.IO

let alphabet = Map.ofList [('A', 0); ('C', 1); ('G', 2); ('T', 3)]

let findMostProbable (s : string) (profile : float [,]) =
    let k = Array2D.length2 profile
    let kmers = [0..s.Length - k].Select(fun i -> s.Substring(i, k)).Distinct().ToArray()
    let probs = 
        kmers 
        |> Array.map 
            (fun kmer -> 
                kmer, kmer.ToCharArray() 
                    |> Array.map (fun c -> alphabet.[c])
                    |> Array.mapi (fun i ind -> profile.[ind, i])
                    |> Array.fold (fun state p -> state * p) 1.)

    let mostProbable = probs.OrderByDescending(fun (kmer, prob) -> prob).GroupBy(fun (kmer, prob) -> prob).First().Select(fun (kmer, prob) -> kmer).ToArray()
    if mostProbable.Length > 1 then
        let minindex = mostProbable.Select(fun kmer -> s.IndexOf(kmer)).Min()
        s.Substring(minindex, k)
    else
        mostProbable.[0]

let p = [|
            [|0.2; 0.2; 0.3; 0.2; 0.3|]
            [|0.4; 0.3; 0.1; 0.5; 0.1|]
            [|0.3; 0.3; 0.5; 0.2; 0.4|]
            [|0.1; 0.2; 0.1; 0.1; 0.2|]
        |]
let s = "ACCTGTTTATTGCCTAAGTTCCGAACAAACCCAATATAGCCCGAGGGCCT"

let findMostProbableFile fileName =
    let lines = File.ReadAllLines(fileName)
    let profs = lines.[2..] |> Array.map(fun s -> s.Trim().Split(' '))
    let profile = array2D profs |> Array2D.map float
    findMostProbable lines.[0] profile