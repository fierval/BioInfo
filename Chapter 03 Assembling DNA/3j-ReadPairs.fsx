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
open System.Diagnostics

let completeEuler (graph : 'a Euler) =
    let edge = findUnbalanced graph
    
    let out, in' = edge
    if not (graph.ContainsKey out) then graph.Add(out, [].ToList())
    graph.[out].Add(in')
    graph, out, in'

let cycleToPath (out : 'a) (in' : 'a) (graph : 'a List) =
    // move head range to tail deals with loops. 
    // here we want to return a path
    let shiftPath graph idx =
        let moved = moveHeadRangeToTail graph idx
        moved.RemoveAt(moved.Count - 1)
        moved

    let idx = findEdgeIndex out in' graph

    if idx < 0 then failwith (" not found")
    shiftPath graph (idx + 1)

let eulerToDebruijn (k : int) (gr : string) =
    //F# 4.0: ctor's as fst class citizens.
    gr.ToCharArray() |> Array.chunkBySize k |> Array.map String |> toString 

let parseAndSplitPairs (pairs : string seq) =
    pairs 
    |> Seq.map 
        (fun s -> 
            let arr = s.Trim().Split([|'|'|])
            arr.[0], arr.[1])
    |> Seq.toList

let completePath k d (prfSuf : (string list * string list)) =
    let prf, suf = prfSuf
    let prefixCommon = prf.[k + d..]
    let suffixCommon = suf.[0..suf.Length - k - d - 1]

    if prefixCommon = suffixCommon then
        (prf @ suf.[suf.Length - k - d..]) |> toString
        else String.Empty

let constructPathFromPairs (arr : string seq) d =
    let sq = parseAndSplitPairs arr
    let graph, out, in' = sq |> debruijnPaired |> completeEuler
    let k = (fst (sq.First())).Length
    let edge = out, in'
    let pairedEulerian = allEulerianPaired edge graph
    pairedEulerian 
    |> Seq.map (fun gr -> gr |> cycleToPath out in' |> Seq.toList |> List.unzip |> completePath k d)
    |> Seq.filter (fun s -> not (String.IsNullOrEmpty s))
    |> Seq.take 1
    |> Seq.exactlyOne

let test () =
    let arr = ["GAGA|TTGA";"TCGT|GATG";"CGTG|ATGT";"TGGT|TGAG";"GTGA|TGTT";"GTGG|GTGA";"TGAG|GTTG";"GGTC|GAGA";"GTCG|AGAT"]
    let d = 2

    let act = constructPathFromPairs arr d
    let exp = "GTGGTCGTGAGATGTTGA"
    let passed = act = exp
    Debug.Assert passed

let name = "rosalind_ba3j.txt"

let solve name =
    let str = File.ReadAllLines name
    let [|k; d|] = str.[0].Trim().Split([|' '|], 2) |> Array.map (fun e -> int e)
    let arr = str.[1..]
    let sol = constructPathFromPairs arr d
    File.WriteAllText(@"c:\temp\fromParis.txt", sol)
