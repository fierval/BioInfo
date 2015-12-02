open System
open System.Linq
open System.IO
open System.Collections.Generic

#load "3f-EulerCycle.fsx"

open ``3f-EulerCycle``

(*
Return a tuple of start -> end for the path, finding unbalanced nodes to form an extra edge
*)
let findUnbalanced (graph : 'a Euler) =
    // outbound
    let groupedOut = 
        graph |> Seq.map(fun kvp -> kvp.Key, kvp.Value.Count) 

    //inbound.
    let groupedIn = 
        graph |> Seq.collect (fun kvp -> kvp.Value) |> Seq.groupBy id |> Seq.map (fun (key, vals) -> key, vals.Count())

    let out = groupedOut.Except groupedIn |> Seq.toArray |> Array.sortBy fst
    let in' = groupedIn.Except groupedOut |> Seq.toArray |> Array.sortBy fst

    if in'.Length > 2 || out.Length > 2 then failwith "Too unbalanced!"

    let pickFromUneven (o : ('a * int) []) (i : ('a * int) []) =
        match o with
        | [|(a, _)|] -> fst (i.Where(fun e -> fst e <> a).Single()), a
        | _ -> fst i.[0], fst (o.Where(fun e -> fst e <> fst i.[0]).Single())
            
    if out.Length = in'.Length && out.Length = 1 then fst (in'.Single()), fst (out.Single())
    elif out.Length = in'.Length then
        if snd out.[0] < snd in'.[0] 
        then fst out.[0], fst out.[1]
        else fst out.[1], fst out.[0]
    else pickFromUneven out in'

let findPath (graph : 'a Euler) (edge : 'a * 'a) =
    let out, in' = edge
    if not (graph.ContainsKey out) then graph.Add(out, List<'a>())
    graph.[out].Add(in')

    let cycle = findCycle graph

    // now find where we are using the new edge inside the cycle
    let mutable stop = false
    let mutable i = 0
    while not stop do
        if cycle.[i] = out && cycle.[i + 1] = in' then stop <- true
        else
            i <- i + 1
            if i = cycle.Count - 2 then stop <- true
    
    cycle.RemoveAt(cycle.Count - 1)
    if i = cycle.Count - 2 then cycle
    else
        let move = cycle.GetRange(0, i + 1)
        cycle.AddRange move
        cycle.RemoveRange(0, i + 1)
        cycle

let name = "rosalind_ba3g.txt"

let solvePath name =
    let strs = File.ReadAllLines name
    let graph = parse strs

    let edge = findUnbalanced graph
    let sol = findPath graph edge |> decorateCycle
    File.WriteAllText(@"c:\temp\euler_path.txt", sol)