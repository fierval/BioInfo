open System
open System.Linq
open System.IO
open System.Collections.Generic

#load @"..\Chapter 3 Molecular Clocks\environment.fsx"

type 'a Euler = Dictionary<'a, List<'a>>
type 'a Edge = 'a * 'a
type 'a Cycle = List<'a>

let parse (s : string []) : int Euler =
    let undec = 
        s 
        |> Array.map(fun e -> 
                        let arr = e.Split([|"->"|], 2, StringSplitOptions.RemoveEmptyEntries)
                        int (arr.[0].Trim()), arr.[1].Split(',').Select(fun el -> int (el.Trim())).ToList())

    undec.ToDictionary(fst, snd)

let findCycles (graph : 'a Euler) =
    let rec findCyclesRec (curCycle : 'a Cycle)  =

        let rec buildCycle (cycle : 'a Cycle)  =
        
            // get new node for the cycle:
            // remove it from the graph
            // keep track of unused edges
            let (|NextNode|_|) node =
                if graph.ContainsKey node then
                    let nextNode = graph.[node].[0]
                    cycle.Add(nextNode)
                    graph.[node].RemoveAt(0)
                    // we don't care about adding existing items to a hash set. It checks for them
                    if graph.[node].Count = 0 
                        then graph.Remove node |> ignore
                    Some nextNode
                else
                    None

            match curCycle.Last() with
                | NextNode nextNode -> buildCycle cycle
                | _ -> cycle.RemoveAt(cycle.Count - 1); cycle
                

        // rearrange the index so that we 
        // have walked the entire index from the start node
        let rearrangeCycleIndex curNode =
            let start = curCycle.IndexOf(curNode)
            let move = curCycle.GetRange(0, start)
            curCycle.AddRange move
            curCycle.RemoveRange(0, start)
            curCycle.Add curNode

        let curNode = curCycle.Last()
        if curCycle.Count > 0 then 
            // walk the cycle starting from the new edge
            rearrangeCycleIndex curNode
            // all unused edges were used

        let cycle = buildCycle curCycle
        if graph.Count = 0 then cycle else findCyclesRec cycle

    let curCycle = List<'a>()
    curCycle.Add (graph.First().Key)
    findCyclesRec curCycle

let name = "euler1.txt"

let solve name =
    let strs = File.ReadAllLines name
    let graph = parse strs

    let curCycle = List<int>()
    let cycle = curCycle

    findCycles graph