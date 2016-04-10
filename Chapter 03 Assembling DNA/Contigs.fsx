open System
open System.Linq
open System.Collections.Generic

Environment.CurrentDirectory <- Environment.GetFolderPath(Environment.SpecialFolder.UserProfile) + "\Downloads"

#load "3f-EulerCycle.fsx"
#load "3g-EulerPath.fsx"
#load "3d-3e-debruin.fsx"
#load "3h-3i-Genome.fsx"
#load "AllEulerian.fsx"
#load "SpanningTree.fsx"

open AllEulerian
open ``3f-EulerCycle``
open ``3g-EulerPath``
open ``3d-3e-debruin``
open ``3h-3i-Genome``
open SpanningTree

let findIsolatedCycles (graph : 'a Euler) =
    let trees = findMaxSpanTrees graph |> Seq.toList
    let revGraph = reverseAdj graph

    let isPossibleLoop (tree : 'a SpanningTree) (graph : 'a Euler) =
        tree |> Seq.map (fun v -> graph.[v]) |> Seq.exists (fun l -> l.Count <> 1) |> not

    let isCycle (tree : 'a SpanningTree) =
        tree.Except graph.Keys |> Seq.isEmpty && 
        tree.Except revGraph.Keys |> Seq.isEmpty &&
        isPossibleLoop tree graph &&
        isPossibleLoop tree revGraph

    [|
        for tree in trees do
            if isCycle tree then yield tree |> Seq.toList
    |]

let findMaxNonBranching (graph : 'a Euler) =
    let revGraph = reverseAdj graph

    let zeroIn = graph.Keys.Except revGraph.Keys
    let moreThanOneOut = 
            graph 
            |> Seq.filter (fun kvp -> kvp.Value.Count > 1) 
            |> Seq.map (fun kvp -> kvp.Key)

    let moreThanOneIn =
        revGraph
        |> Seq.filter (fun kvp -> kvp.Value.Count > 1)
        |> Seq.map (fun kvp -> kvp.Key)

    let crossRoads = zeroIn.Union moreThanOneOut |> fun u -> u.Union moreThanOneIn |> HashSet

    [|
        for start in crossRoads do
            let next = graph.[start]
            let contig = [start].ToList()
            for v in next do
                yield [
                    yield start
                    let mutable x = v
                    let mutable stop = false
                    while not stop do
                        yield x
                        stop <- not (graph.ContainsKey x) || crossRoads.Contains x
                        if not stop then x <- graph.[x].Single()
                ]
    |]

let findContigs (arr : string seq) =
    let graph = debruijn arr
    let fromNonLoops = async {return findMaxNonBranching graph}
    let fromLoops = async {return findIsolatedCycles graph}
    seq {yield fromNonLoops; yield fromLoops} 
    |> Async.Parallel 
    |> Async.RunSynchronously 
    |> Array.concat
    |> Array.map toString


let arr = ["ATG"; "ATG"; "TGT"; "TGG"; "CAT"; "GGA"; "GAT"; "AGA"]
findContigs arr