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
open System.IO

let findIsolatedCycles (graph : 'a Euler) (appendFirst : bool) =
    let trees = findMaxSpanTrees graph |> Seq.toList
    let revGraph = reverseAdj graph

    let isPossibleLoop (tree : 'a SpanningTree) (graph : 'a Euler) =
        tree |> Seq.map (fun v -> graph.[v]) |> Seq.exists (fun l -> l.Count <> 1) |> not

    let isCycle (tree : 'a SpanningTree) =
        tree.Except graph.Keys |> Seq.isEmpty && 
        tree.Except revGraph.Keys |> Seq.isEmpty &&
        isPossibleLoop tree graph &&
        isPossibleLoop tree revGraph

    let walkCycle (tree : 'a SpanningTree) =
        let rec moveNext (cycle : 'a list) frst =
            if frst = graph.[cycle.Head].Single() then (if appendFirst then frst::cycle else cycle) |> List.rev
            else
                moveNext (graph.[cycle.Head].Single()::cycle) frst

        moveNext [tree.First()] (tree.First())
    [|
        for tree in trees do
            if isCycle tree then 
                yield tree |> walkCycle
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

let toStringFromInt (l : int seq) = 
    Seq.fold (fun acc e -> acc + " -> " + e.ToString()) String.Empty l
    |> fun s -> s.[4..]

let findContigs (graph : 'a Euler) (decorator : 'a seq -> string) appendFirst =
    let fromNonLoops = async {return findMaxNonBranching graph}
    let fromLoops = async {return findIsolatedCycles graph appendFirst}
    seq {yield fromNonLoops; yield fromLoops} 
    |> Async.Parallel 
    |> Async.RunSynchronously 
    |> Array.concat
    |> Array.map decorator


let arr = [|"1 -> 2"; "2 -> 3"; "3 -> 4,5"; "6 -> 7"; "7 -> 6"|]

let name = @"C:\Users\boris\Downloads\maximal_nonbranching_paths.txt"
let out = Some @"C:\Users\boris\Downloads\maximal_nonbranching_out.txt"

let solveTest (out : string option) name =
    let test, out = 
        match out with
        | Some out -> true, out
        | None -> false, ""

    if test && not (File.Exists out) then failwith "Output does not exist"
    if not (File.Exists name) then failwith "Input does not exist"

    let graph = File.ReadAllLines name |> parse
    //let graph = File.ReadAllLines |> debruijn

    let actual = findContigs graph toStringFromInt true
    
    if test then
        let expected = File.ReadAllLines out
        actual.Length = expected.Length && actual.Except expected |> Seq.isEmpty && expected.Except actual |> Seq.isEmpty
    else
        File.WriteAllLines(@"c:\temp\contigs.txt", actual)
        true

let solve = solveTest None