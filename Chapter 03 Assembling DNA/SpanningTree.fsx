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
    // create a spanning tree, starting from a vertex
    let createTree (graph : 'a Euler) (revGraph : 'a Euler) start =
        let fromIntoVert v =
            let fromStart = graph.[v]
            let intoStart = revGraph.[v]
            fromStart.Union intoStart

        let spanning = HashSet(fromIntoVert start)
        let mutable prevCount = spanning.Count
        let mutable isFirst = true

        let fromStart = graph.[start]

        let rec appendToSpanning (spanning : HashSet<'a>) prevCount =
            if prevCount = spanning.Count then spanning
            else
                let prevCount = spanning.Count
                spanning
                |> Seq.map (fun v -> fromIntoVert v)
                |> Seq.collect id
                |> Seq.iter (spanning.Add>>ignore)
                appendToSpanning spanning prevCount

        appendToSpanning spanning 0

    let revGraph = reverseAdj graph
    let mutable stop = false
    let mutable remaining = HashSet(graph.Keys)

    seq {
        while not (remaining |> Seq.isEmpty) do
            let start = remaining.First()
            let maxTree = createTree graph revGraph start
            remaining <- remaining.Except maxTree |> HashSet
            yield maxTree
    }
