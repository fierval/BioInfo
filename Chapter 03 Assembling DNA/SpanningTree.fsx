open System
open System.Linq
open System.Collections.Generic

Environment.CurrentDirectory <- Environment.GetFolderPath(Environment.SpecialFolder.UserProfile) + "\Downloads"

#load "3f-EulerCycle.fsx"
#load "3g-EulerPath.fsx"
#load "3d-3e-debruin.fsx"
#load "3h-3i-Genome.fsx"
#load "AllEulerian.fsx"

open AllEulerian
open ``3f-EulerCycle``
open ``3g-EulerPath``
open ``3d-3e-debruin``
open ``3h-3i-Genome``

type 'a SpanningTree = 'a HashSet

let findMaxSpanTrees (graph : 'a Euler) : SpanningTree<'a> seq =
    let revGraph = reverseAdj graph
    let mutable remaining = HashSet(graph.Keys)
    let allKeys = revGraph.Keys.Union graph.Keys |> Seq.toList

    // create a spanning tree, starting from a vertex
    let createTree (graph : 'a Euler) (revGraph : 'a Euler) start =
        let fromIntoVert v =
            let fromStart = if graph.ContainsKey v then graph.[v] else List()
            let intoStart = if revGraph.ContainsKey v then revGraph.[v] else List()
            fromStart.Union intoStart

        let spanning = HashSet(fromIntoVert start)

        let rec appendToSpanning (spanning : HashSet<'a>) prevCount =
            if spanning.Count = allKeys.Length || prevCount = spanning.Count then spanning
            else
                let canReach = 
                    spanning
                    |> Seq.map (fun v -> fromIntoVert v)
                    |> Seq.collect id
                let newSpanning = canReach |> fun s -> s.Union spanning |> HashSet
                appendToSpanning newSpanning spanning.Count

        appendToSpanning spanning 0

    seq {
        while not (remaining |> Seq.isEmpty) do
            let start = remaining.First()
            let maxTree = createTree graph revGraph start
            remaining <- remaining.Except maxTree |> HashSet
            yield maxTree
    }

let arr = ["ATG"; "ATG"; "TGT"; "TGG"; "CAT"; "GGA"; "GAT"; "AGA"; "UVX"; "VXT"; "XTU"; "TUV"]
let graph = debruijn arr
