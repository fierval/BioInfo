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

(* An "reverse" adjacency list of in -> out *)
let reverseAdj (graph : string Euler) =
    let gr = graph.Where(fun v -> v.Value.Count > 1)
    if gr.FirstOrDefault() = Unchecked.defaultof<KeyValuePair<string, List<string>>> then Unchecked.defaultof<string Euler>
    else
        gr
            .SelectMany(fun kvp -> seq {for v in kvp.Value -> (kvp.Key, v)})
            .GroupBy(fun (o, i) -> i)
            .ToDictionary((fun gr -> gr.Key), (fun (gr : IGrouping<string, string * string>) -> gr.Select(fun (o, i) -> i).ToList()))

let cloneDict (dct : Dictionary<'a, 'b>) =
    dct.Select(id).ToDictionary((fun kvp -> kvp.Key), (fun (kvp : KeyValuePair<'a, 'b>) -> kvp.Value))

let allEulerian (graph : string Euler) =
    let allCycles = List<string Euler * string Euler>()
    let revGraph = reverseAdj graph

    allCycles.Add(graph, revGraph)

    let isDone () = snd (allCycles.First()) = Unchecked.defaultof<string Euler>
    let isConnected (gr : string Euler) = gr.FirstOrDefault(fun gr -> gr.Value.Count = 0) = Unchecked.defaultof<KeyValuePair<string, List<string>>>

    while not (isDone ()) do
        let curGraph, revCurGraph = allCycles.First()
        allCycles.RemoveAt(0)
        for v in revCurGraph.Keys do //for each in-edge (u, v) into v
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
                                            newVert.Add(x)
                                            newGraph.Add(x, newVert)
                                            newGraph.[u].Add(x)
                                            newGraph.[v].RemoveAt(newGraph.[v].IndexOf w)
                                            if newRevGraph.ContainsKey w then
                                                newRevGraph.[w].RemoveAt(newRevGraph.[w].IndexOf v)
                                            newGraph, newRevGraph               
                                ).AsEnumerable()).Where(fun (gr, rev) -> isConnected gr)
            allCycles.AddRange newGraphs

