open System
open System.Collections.Generic
open System.Linq

#load "AllEulerian.fsx"

open ``3f-EulerCycle``
open ``3g-EulerPath``
open ``3d-3e-debruin``
open ``3h-3i-Genome``
open AllEulerian

open System.IO

let completeEuler (graph : string Euler) =
    let edge = findUnbalanced graph
    
    let out, in' = edge
    if not (graph.ContainsKey out) then graph.Add(out, [].ToList())
    graph.[out].Add(in')
    graph, out, in'

let cycleToPath (out : string) (in' : string) (graph : string) =
    let k = out.Length
    let edge = out + in'
    let idx = graph.IndexOf edge + k
    if idx = 0 then failwith (edge + " not found")
    graph.[idx..graph.Length - k - 1] + graph.[0..idx-1]

let eulerToDebruijn (k : int) (gr : string) =
    //F# 4.0: ctor's as fst class citizens.
    gr.ToCharArray() |> Array.chunkBySize k |> Array.map String |> toString 

let prep (nucleotides : string seq) =
    nucleotides |> debruijn |> completeEuler //|> findPath |> toString

let parseAndSplitPairs (pairs : string seq) =
    pairs 
    |> Seq.map 
        (fun s -> 
            let arr = s.Trim().Split([|'|'|])
            arr.[0], arr.[1])
    |> Seq.toList
    |> List.unzip    

let reconstructPath (arr : string seq) d =
    let pref, suff = parseAndSplitPairs arr

    let prefPaths, outPref, inPref = prep pref
    let suffPaths, outSuff, inSuff = prep suff
    let k = pref.[0].Length

    let completePath (prefPath : string) (suffPath : string) =
        let prefixCommon = prefPath.Substring(k + d)
        let suffixCommon = suffPath.Substring(0, suffPath.Length - k - d)

        if prefixCommon = suffixCommon then
            prefPath.Substring(0, k + d) + suffPath 
            else ""

    let allPrefs = allEulerian prefPaths
    let allSuffs = allEulerian suffPaths

    let mutable res = String.Empty
    let mutable stop = false
    while not stop do
        for p in allPrefs |> Seq.toList do
            let pp = (cycleToPath outPref inPref >> eulerToDebruijn (k - 1)) p

            for s in allSuffs do
                let ss = (cycleToPath outPref inPref >> eulerToDebruijn (k - 1)) s
                res <- completePath pp ss
                if not (String.IsNullOrEmpty res) then stop <- true
        stop <- true
    res

let arr = ["GAGA|TTGA";"TCGT|GATG";"CGTG|ATGT";"TGGT|TGAG";"GTGA|TGTT";"GTGG|GTGA";"TGAG|GTTG";"GGTC|GAGA";"GTCG|AGAT"]
let d = 2

let name = @"C:\Users\boris\Downloads\string_reconstruction_from_read_pairs.txt"

let solve name =
    let str = File.ReadAllLines name
    let [|k; d|] = str.[0].Trim().Split([|' '|], 2) |> Array.map (fun e -> int e)
    let sol = reconstructPath (str.[1..]) d
    File.WriteAllText(@"c:\temp\fromParis.txt", sol)
