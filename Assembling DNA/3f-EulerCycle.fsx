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
    let rec findCyclesRec (curCycle : 'a Cycle)  (unusedNodes : 'a HashSet) =

        let rec buildCycle (cycle : 'a Cycle) =
        
            let (|NextNode|_|) node =
                if graph.ContainsKey node then
                    Some graph.[node].[0]
                else
                    None

            // get new node for the cycle:
            // remove it from the graph
            // keep track of unused edges
            let node = curCycle.Last()
            match node with
                | NextNode nextNode -> 
                    cycle.Add(nextNode)
                    graph.[node].RemoveAt(0)
                    // we don't care about adding existing items to a hash set. It checks for them
                    if graph.[node].Count = 0 
                        then 
                            graph.Remove node |> ignore
                            unusedNodes.Remove node |> ignore
                        else 
                            unusedNodes.Add node |> ignore

                    buildCycle cycle
                | _ -> 
                    cycle.RemoveAt(cycle.Count - 1)
                    cycle

        // rearrange the index so that we 
        // have walked the entire index from the start node
        let rearrangeCycleIndex curNode =
            let start = curCycle.IndexOf(curNode)
            let move = curCycle.GetRange(0, start)
            curCycle.AddRange move
            curCycle.RemoveRange(0, start)
            curCycle.Add curNode

        let curNode = if unusedNodes.Count = 0 then  curCycle.Last() else unusedNodes.First()
        if curCycle.Count > 1 then 
            // walk the cycle starting from the new edge
            rearrangeCycleIndex curNode

        let curCycle = buildCycle curCycle
        if graph.Count = 0 then 
            curCycle.Add(curCycle.First()) //close the loop
            curCycle
            else 
                findCyclesRec curCycle unusedNodes

    let curCycle = List<'a>()
    curCycle.Add (graph.First().Key)
    let unusedNodes = HashSet<'a>()
    findCyclesRec curCycle unusedNodes

let name = "rosalind_ba3f.txt"

let decorateCycle (cycle : 'a seq) =
    let str = cycle |> Seq.fold (fun acc e -> acc + "->" + e.ToString()) String.Empty
    str.Substring(2)

// walk the graph to make sure we are good.
let verifyAnswer (graph : 'a Euler) (cycle : 'a List) =
    for i = 0 to cycle.Count - 2 do
        let node = cycle.[i]
        let nextNode = cycle.[i + 1]
        if not (graph.ContainsKey node) then failwith (String.Format("Wrong node: {0}", node))
        elif not (graph.ContainsKey nextNode) && i < cycle.Count - 2 then failwith (String.Format("Walking twice: {0} -> {1}", node,nextNode))
        elif graph.[node].IndexOf nextNode < 0 then failwith (String.Format("Edge not found: {0} -> {1}", node, nextNode))
        else
            let ind = graph.[node].IndexOf nextNode
            graph.[node].RemoveAt(ind)
            if graph.[node].Count = 0 then graph.Remove(node) |> ignore
    
    graph

let solve name =
    let sol = File.ReadAllLines name |> parse |> findCycles |> decorateCycle
    File.WriteAllText(@"c:\temp\euler_cycle.txt", sol)