//#load @"..\packages\MathNet.Numerics.FSharp.Signed.3.6.0\MathNet.Numerics.fsx"

//open MathNet.Numerics.LinearAlgebra
#load "environment.fsx"

open System.Linq
open System.IO

let alphabet = dict [('A', 0); ('C', 1); ('G', 2); ('T', 3)]

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

    fst (probs |> Array.maxBy(fun (kmer, prob) -> prob))

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
    let profile = Array2D.init 4 (int lines.[1]) (fun i j -> float profs.[i].[j])
    findMostProbable lines.[0] profile