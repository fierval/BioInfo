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
    graph.[out].Add(in')
    graph, out, in'

let cycleToPath (out : string) (in' : string) (graph : string) =
    let edge = out + in'
    let idx = graph.IndexOf edge + 1
    if idx = 0 then failwith (edge + " not found")
    graph.[idx..graph.Length - 2] + graph.[0..idx-1]

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
        let prefixCommon = prefPath.Substring(k + d + 1)
        let suffixCommon = suffPath.Substring(0, suffPath.Length - k - d - 1)

        if prefixCommon = suffixCommon then
            let res = prefPath.Substring(0, k + d) + suffPath 
            res.[0..res.Length - 2]
            else ""

    let allPrefs = allEulerian prefPaths |> List.map (cycleToPath outPref inPref)
    let allSuffs = allEulerian suffPaths |> List.map (cycleToPath outSuff inSuff)

    let mutable res = String.Empty
    let mutable stop = false
    while not stop do
        for i in [0..allPrefs.Count() - 1] do
            for j in [0..allSuffs.Count() - 1] do
                res <- completePath allPrefs.[i] allSuffs.[j]
                if not (String.IsNullOrEmpty res) then stop <- true
    res

let arr = ["GAGA|TTGA";"TCGT|GATG";"CGTG|ATGT";"TGGT|TGAG";"GTGA|TGTT";"GTGG|GTGA";"TGAG|GTTG";"GGTC|GAGA";"GTCG|AGAT"]
let d = 2

let name = "rosalind_ba3j.txt"

let solve name =
    let str = File.ReadAllLines name
    let [|k; d|] = str.[0].Trim().Split([|' '|], 2) |> Array.map (fun e -> int e)
    let sol = reconstructPath (str.[1..]) d
    File.WriteAllText(@"c:\temp\fromParis.txt", sol)