open System
open System.Linq
open System.IO
open System.Collections.Generic

type 'a Euler = Dictionary<'a, List<'a>>
type 'a Cycle = List<'a * 'a>

let parse (s : string []) : int Euler =
    let undec = 
        s 
        |> Array.map(fun e -> 
                        let arr = e.Split([|" "; "->"|], 2, StringSplitOptions.RemoveEmptyEntries)
                        int arr.[0], arr.[2].Split(',').Select(fun el -> int (el.Trim())).ToList())

    undec.ToDictionary((fun (k, v) -> k),(fun (k, v) -> v))

let findCycles (graph : 'a Euler) =
    let cycle : 'a Cycle = List<'a * 'a>()

    let rec buildCycle (graph : 'a Euler) (cycle : 'a Cycle) (curEdge : 'a) =

        let addEdge edge =
            let nextEdge = graph.[edge].First()
            cycle.Add(edge, nextEdge)
            graph.[edge].RemoveAt(0)
            if graph.[edge].Length = 0 then graph.Remove(edge)

        if graph.Count = 0 then cycle
        else
            let nextEdge =
                if graph.ContainsKey curEdge then addEdge curEdge
                 
                
