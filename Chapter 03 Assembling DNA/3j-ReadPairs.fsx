open System
open System.Collections.Generic
open System.Linq

#load "AllEulerian.fsx"
#load "3c-OverlapGraph.fsx"

open ``3f-EulerCycle``
open ``3g-EulerPath``
open ``3d-3e-debruin``
open ``3h-3i-Genome``
open AllEulerian
open ``3c-OverlapGraph``

open System.IO

let completeEuler (graph : string Euler) =
    let edge = findUnbalanced graph
    
    let out, in' = edge
    if prefix out <> suffix in' then failwith "cannot complete graph"
    if not (graph.ContainsKey out) then graph.Add(out, [].ToList())
    graph.[out].Add(in')
    graph, out, in'

let moveHeadRangeToTail (graph : 'a List) idx =
    graph.RemoveAt(graph.Count - 1)
    let head = graph.GetRange(0, idx)
    graph.RemoveRange(0, idx)
    graph.AddRange(head)
    graph

let cycleToPath (out : 'a) (in' : 'a) (graph : 'a List) =
    let idx = graph |> Seq.windowed 2 |> Seq.findIndex (fun [|f; s|] -> f = out && s = in')
    if idx < 0 then failwith (" not found")
    moveHeadRangeToTail graph idx

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

    let completePath (prefPath : List<string>) (suffPath : List<string>) =
        let prf = prefPath |> Seq.toList
        let suf = suffPath |> Seq.toList

        let prefixCommon = prf.[k + d..]
        let suffixCommon = suf.[0..suf.Length - k - d]

        if prefixCommon = suffixCommon then
            (pref.[0..k + d] @ suf) |> toString
            else String.Empty

    let allPrefs = allEulerianInt prefPaths
    let allSuffs = allEulerianInt suffPaths

    let mutable res = String.Empty
    let mutable stop = false
    while not stop do
        for p in allPrefs |> Seq.toList do
            let pp = cycleToPath outPref inPref p

            for s in allSuffs do
                let ss = cycleToPath outPref inPref s
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
