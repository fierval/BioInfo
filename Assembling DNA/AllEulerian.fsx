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
            .ToDictionary((fun gr -> gr.Key), (fun (gr : IGrouping<string, string * string>) -> gr.Select(fun (o, i) -> i).ToList()))

let cloneDict (dct : Dictionary<'a, 'b>) =
    dct.Select(id).ToDictionary((fun kvp -> kvp.Key), (fun (kvp : KeyValuePair<'a, 'b>) -> kvp.Value))

let allEulerian (graph : string Euler) =
    let allCycles = List<string Euler * string Euler>()
    let revGraph = reverseAdj graph

    allCycles.Add(graph, revGraph)

    let isDone () =
        let curGraph, revCurGraph = allCycles.First()
        curGraph.LongCount(fun kvp -> kvp.Value.Count > 1) = 0L

    while not (isDone ()) do
        let curGraph, revCurGraph = allCycles.First()
        let outVertices = revCurGraph.Where(fun kvp -> kvp.Value.Count > 1).Select(fun kvp -> kvp.Key)
        allCycles.RemoveAt(0)
        for v in outVertices do //for each in-edge (u, v) into v
            let u's = revCurGraph.[v] // all edges u coming into v
            let w's = curGraph.[v] // for each out edge v, w 
            let newGraphs = 
                u's
                    .SelectMany(fun u i -> 
                                let newGraph = cloneDict curGraph
                                let newRevGraph = cloneDict revCurGraph
                                w's.Select(fun w j -> 
                                            let x = v + "_" + i.ToString() + "_" + j.ToString()
                                            let newVert = List<string>()
                                            newVert.Add(w)
                                            newGraph.Add(x, newVert)
                                            newGraph.[u].Add(x)
                                            newGraph.[v].RemoveAt(newGraph.[v].IndexOf w)
                                            newGraph.[u].RemoveAt(newGraph.[u].IndexOf v)
                                            if newGraph.[u].Count = 0 || newGraph.[v].Count = 0 then Unchecked.defaultof<string Euler>, Unchecked.defaultof<string Euler>

                                            else
                                                newRevGraph.[w].RemoveAt(newRevGraph.[w].IndexOf v)
                                                newRevGraph.[v].RemoveAt(newRevGraph.[v].IndexOf u)
                                                if newRevGraph.[v].Count = 0 then newRevGraph.Remove(v) |> ignore
                                                newGraph, newRevGraph               
                                ).AsEnumerable()).Where(fun (gr, rev) -> rev <> Unchecked.defaultof<string Euler>)
            allCycles.AddRange newGraphs

let graph = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, @"all_eulerian.txt")) |> parseStr