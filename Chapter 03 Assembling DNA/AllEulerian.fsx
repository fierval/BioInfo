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

let findEdgeIndex (out : 'a) (in' : 'a) (graph : 'a seq) =
    graph |> Seq.windowed 2 |> Seq.findIndex (fun [|f; s|] -> f = out && s = in')
    
let moveHeadRangeToTail (graph : 'a seq) idx =
    // assuming we have a closed loop
    let grLst = graph.ToList()
    grLst.RemoveAt(grLst.Count - 1)
    let head = grLst.GetRange(0, idx)
    grLst.RemoveRange(0, idx)
    grLst.AddRange(head)
    grLst.Add(grLst.First())
    grLst

let parseStr (s : string []) : string Euler =
    let undec = 
        s 
        |> Array.map(fun e -> 
                        let arr = e.Split([|"->"|], 2, StringSplitOptions.RemoveEmptyEntries)
                        arr.[0].Trim(), arr.[1].Split(',').Select(fun el -> el.Trim()).ToList())

    undec.ToDictionary(fst, snd)

(* A "reverse" adjacency list of in -> out *)
let reverseAdj (graph : 'a Euler) =
        graph
            .SelectMany(fun kvp -> seq {for v in kvp.Value -> (kvp.Key, v)})
            .GroupBy(fun (o, i) -> i)
            .ToDictionary((fun gr -> gr.Key), (fun (gr : IGrouping<'a, 'a * 'a>) -> gr.Select(fun (o, i) -> o).ToList()))

(*deep copy a graph*)
let cloneDict (dct : 'a Euler) =
    dct.Select(fun kvp -> new KeyValuePair<'a, List<'a>>(kvp.Key, kvp.Value.Select(id).ToList())).ToDictionary((fun kvp -> kvp.Key), (fun (kvp : KeyValuePair<'a, List<'a>>) -> kvp.Value))

let  walk (comp : 'a -> 'a) (gr : 'a Euler) =
    let start = gr.First().Key
    let mutable next = start
    let mutable count = 0
    let loop = List<'a>()
    while count = 0 || next <> start do
        loop.Add(comp next)
        next <- gr.[next].Single()
        count <- count + 1
    loop.Add(start) //close the loop
    loop

let isConnectedGraph (gr : 'a Euler) =
    let start = gr.First().Key
    
    let visited = HashSet<'a>([start])
    let queue = Queue<'a>()
    queue.Enqueue(start)
    
    while queue.Count > 0 do
        let cur = queue.Dequeue()
        let next = gr.[cur]
        let toVisit = next |> Seq.filter (fun e -> not (visited.Contains e)) |> Seq.toList
        for notVisited in toVisit do
            visited.Add(notVisited) |> ignore
            queue.Enqueue(notVisited)
                
    gr.Keys.Count = visited.Count

// compare two eulerian cycles
// cycles are represented as lists
// TODO: really bad side effects! changes both lists
let areEq (lstA : 'a List) (lstB : 'a List) (edge : 'a * 'a)=
    if lstA.Count <> lstB.Count then false
    else
        let o, i = edge
        let idxUniqueEdge = findEdgeIndex o i lstA
        let newA = moveHeadRangeToTail lstA (idxUniqueEdge + 1)

        let idxUniqueEdgeB = findEdgeIndex o i lstB
        if idxUniqueEdgeB < 0 then false
        else
            let newB = moveHeadRangeToTail lstB (idxUniqueEdgeB + 1)
            Seq.fold (&&) true (Seq.zip newA newB |> Seq.map (fun (aa,bb) -> aa=bb))

let isPossibleLoop (gr : 'a Euler) =
    not (gr |> Seq.exists (fun kvp -> kvp.Value.Count > 1))

type 'a NewVertexGenerator = int -> int -> 'a -> 'a

// Get all eulerian cycles within an almost-balanced graph.
// First argument defines how a new vertex is generated.
let allEulerian<'a when 'a : equality> (newVertex: 'a NewVertexGenerator) (comp : 'a -> 'a) (edge : 'a * 'a) (graph : 'a Euler) =
    let allCycles = List<'a Euler * 'a Euler>()
    let allLoops = List<List<'a>>()
    let revGraph = reverseAdj graph

    if isPossibleLoop graph then allLoops.Add(walk comp graph)
    else
        allCycles.Add(graph, revGraph)

        let isDone () =
            allCycles.Count = 0 || (
                                    let curGraph, revCurGraph = allCycles.First()
                                    isPossibleLoop curGraph
            )
            
        while not (isDone ()) do
            let curGraph, revCurGraph = allCycles.First()
            allCycles.RemoveAt(0)

            let outVertices = revCurGraph.Where(fun kvp -> kvp.Value.Count > 1).Select(fun kvp -> kvp.Key)

            for v in outVertices do //for each in-edge (u, v) into v
                for i, u in revCurGraph.[v] |> Seq.mapi (fun i e -> (i, e)) do// all edges u coming into v
                    for j, w in curGraph.[v] |> Seq.mapi (fun i e -> (i, e)) do // for each out edge v, w 
                        let newGraph = cloneDict curGraph
                        let newRevGraph = cloneDict revCurGraph
                        let x = newVertex i j v
                        newGraph.Add(x, [w].ToList())
                        newGraph.[u].Add(x)
                        newGraph.[v].RemoveAt(newGraph.[v].IndexOf w)
                        newGraph.[u].RemoveAt(newGraph.[u].IndexOf v)

                        newRevGraph.[w].RemoveAt(newRevGraph.[w].IndexOf v)
                        newRevGraph.[v].RemoveAt(newRevGraph.[v].IndexOf u)
                        newRevGraph.Add(x, [u].ToList())
                        newRevGraph.[w].Add(x)
                        if isConnectedGraph newGraph then
                            if isPossibleLoop newGraph then
                                let la = walk comp newGraph
                                if not (allLoops |> Seq.exists (fun e -> areEq e la edge)) then 
                                    allLoops.Add(la)
                            else
                                allCycles.Add(newGraph, newRevGraph)               
    allLoops |> Seq.toList

// convert to a differnt type of graph given a map between vertices of different types
let convGraph (graph : 'a Euler) (convMap : Dictionary<'a, 'b>) =
    let mapList (lst : 'a List) =
        lst |> Seq.map (fun e -> convMap.[e]) |> fun s -> s.ToList()

    let convertedGraph = graph |> Seq.map (fun kvp -> convMap.[kvp.Key], mapList kvp.Value) |> fun sq -> sq.ToDictionary(fst, snd)
    convertedGraph

// returns a real cycle
let convCycles (convMap : Map<'a, 'b>) (cycle : 'a List)=
    cycle |> Seq.map (fun e -> convMap.[e]) |> fun sq -> sq.ToList()

type PairedVals<'a> = 'a * 'a
let allEulerianPaired edge (graph : PairedVals<string> Euler) =
    let o, i = edge
    
    let newStrVertex i j (v : PairedVals<string>) = 
        let suff = "_" + string i + "_" + string j
        let frst, scnd = v
        (frst + suff), (scnd + suff)

    let compStr (v : PairedVals<string>) =
        let first, scnd = v // since both original strings are of the same length, it's enough to search one of them for "_"
        let idx = first.IndexOf '_'
        if idx < 0 then v
        else
            first.[0..idx - 1], scnd.[0..idx - 1]

    allEulerian newStrVertex compStr edge graph
    |> Seq.map (fun s -> s |> Seq.map compStr |> fun s -> s.ToList())

// TODO: Memory optimization. For our purposes, the new ineger vertex is generated from the current max one
// so the function has side effects...
let allEulerianInt edge (graph : 'a Euler) =
    let o, i = edge
    let fwdMap = graph.Keys |> Seq.distinct |> fun sq -> sq.ToList()
    let bckwdMap = fwdMap |> Seq.mapi (fun i e -> (e, i)) |> fun sq -> sq.ToDictionary(fst, snd)
     
    let intGraph = convGraph graph bckwdMap
    let newIntVertex _ _ v =
        let strV = fwdMap.[v]
        let vNext = fwdMap.Count
        fwdMap.Add strV
        vNext

    let compInt e = bckwdMap.[fwdMap.[e]]

    let outp = allEulerian newIntVertex compInt (bckwdMap.[o], bckwdMap.[i]) intGraph

    let fwdDict = fwdMap |> Seq.mapi (fun i e -> (i, e)) |> Map.ofSeq
    
    outp |> Seq.map (convCycles fwdDict)