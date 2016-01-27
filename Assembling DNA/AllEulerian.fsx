open System
open System.Collections.Generic
open System.Linq

#load "3f-EulerCycle.fsx"
#load "3g-EulerPath.fsx"
#load "3d-3e-debruin.fsx"
#load "3h-3i-Genome.fsx"

open ``3f-EulerCycle``
open ``3g-EulerPath``
open ``3d-3e-debruin``
open ``3h-3i-Genome``
open System.IO

let parseStr (s : string []) : string Euler =
    let undec = 
        s 
        |> Array.map(fun e -> 
                        let arr = e.Split([|"->"|], 2, StringSplitOptions.RemoveEmptyEntries)
                        arr.[0].Trim(), arr.[1].Split(',').Select(fun el -> el.Trim()).ToList())

    undec.ToDictionary(fst, snd)

(* An "reverse" adjacency list of in -> out *)
let reverseAdj (graph : string Euler) =
        graph
            .SelectMany(fun kvp -> seq {for v in kvp.Value -> (kvp.Key, v)})
            .GroupBy(fun (o, i) -> i)
            .ToDictionary((fun gr -> gr.Key), (fun (gr : IGrouping<string, string * string>) -> gr.Select(fun (o, i) -> o).ToList()))

(*deep copy a graph*)
let cloneDict (dct : string Euler) =
    dct.Select(fun kvp -> new KeyValuePair<string, List<string>>(kvp.Key, kvp.Value.Select(id).ToList())).ToDictionary((fun kvp -> kvp.Key), (fun (kvp : KeyValuePair<string, List<string>>) -> kvp.Value))

let allEulerian (graph : string Euler) =
    let allCycles = List<string Euler * string Euler>()
    let revGraph = reverseAdj graph

    allCycles.Add(graph, revGraph)

    let isDone () =
        let curGraph, revCurGraph = allCycles.First()
        curGraph.LongCount(fun kvp -> kvp.Value.Count > 1) = 0L
    
    let isConnected (gr : string Euler) =
        let start = gr.First().Key
        let mutable next = start
        let mutable count = 0
        while count = 0 || next <> start do
            next <- gr.[next].Single()
            count <- count + 1
        count = gr.Count

    while not (isDone ()) do
        let curGraph, revCurGraph = allCycles.First()
        let outVertices = revCurGraph.Where(fun kvp -> kvp.Value.Count > 1).Select(fun kvp -> kvp.Key)
        allCycles.RemoveAt(0)
        for v in outVertices do //for each in-edge (u, v) into v
            for i, u in revCurGraph.[v] |> Seq.mapi (fun i e -> (i, e)) do// all edges u coming into v
                for j, w in curGraph.[v] |> Seq.mapi (fun i e -> (i, e)) do // for each out edge v, w 
                    let newGraph = cloneDict curGraph
                    let newRevGraph = cloneDict revCurGraph
                    let x = v + "_" + i.ToString() + "_" + j.ToString()
                    newGraph.Add(x, [w].ToList())
                    newGraph.[u].Add(x)
                    newGraph.[v].RemoveAt(newGraph.[v].IndexOf w)
                    newGraph.[u].RemoveAt(newGraph.[u].IndexOf v)

                    newRevGraph.[w].RemoveAt(newRevGraph.[w].IndexOf v)
                    newRevGraph.[v].RemoveAt(newRevGraph.[v].IndexOf u)
                    newRevGraph.Add(x, [u].ToList())
                    newRevGraph.[w].Add(x)

                    allCycles.Add(newGraph, newRevGraph)               

    allCycles |> Seq.filter (fun gr -> isConnected (fst gr)) |> Seq.toList |> List.unzip |> fst                            

let graph = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, @"all_eulerian.txt")) |> parseStr