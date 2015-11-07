open System
open System.Linq
open System.IO
open System.Collections.Generic

type 'a Euler = Dictionary<'a, List<'a>>
type 'a Edge = 'a * 'a
type 'a Cycle = Dictionary<'a, 'a>

// how to follow the cyclic dictionary
type 'a Index = Dictionary<'a, int>

let parse (s : string []) : int Euler =
    let undec = 
        s 
        |> Array.map(fun e -> 
                        let arr = e.Split([|" "; "->"|], 2, StringSplitOptions.RemoveEmptyEntries)
                        int arr.[0], arr.[2].Split(',').Select(fun el -> int (el.Trim())).ToList())

    undec.ToDictionary(fst, snd)

let findCycles (graph : 'a Euler) =
    let rec buildCycle (cycle : 'a Cycle) (curNode : 'a) (nodesUnusedEdges : HashSet<'a>) (index : 'a Index) =

        // get new node for the cycle:
        // remove it from the graph
        // keep track of unused edges
        let getEdge node =
            let nextNode = graph.[node].[0]
            cycle.Add(node, nextNode)
            graph.[node].RemoveAt(0)
            // we don't care about adding existing items to a hash set. It checks for them
            if graph.[node].Count = 0 
                then graph.Remove node |> ignore
                else nodesUnusedEdges.Add node |> ignore
            nextNode

        let nextNode = getEdge curNode
        index.Add(curNode, index.Count)
        if nextNode = curNode then cycle else buildCycle cycle nextNode nodesUnusedEdges index
    
    let rec findCyclesRec (curCycle : 'a Cycle) (nodesUnusedEdges : HashSet<'a>) (index : 'a Index) =
        // rearrange the index so that we 
        // have walked the entire index from the start node
        let rearrangeCycleIndex startNode =
            let mutable nextNode = startNode
            for i = 0 to curCycle.Count do
                index.[nextNode] <- i
                nextNode <- curCycle.[nextNode]
                
        if curCycle.Count > 0 then 
            let startNode = nodesUnusedEdges.First()
            // walk the cycle starting from the new edge
            rearrangeCycleIndex startNode
            // all unused edges were used
            if not (graph.ContainsKey startNode) then (nodesUnusedEdges.Remove startNode |> ignore)

        let cycle = buildCycle curCycle (nodesUnusedEdges.First()) nodesUnusedEdges index
        if graph.Count = 0 then (cycle, index) else findCyclesRec cycle nodesUnusedEdges index

    let initUnused = HashSet<'a>()
    initUnused.Add(graph.First().Key) |> ignore
    findCyclesRec (Dictionary<'a, 'a>()) initUnused (Dictionary<'a, int>())