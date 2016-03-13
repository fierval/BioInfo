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
let reverseAdj (graph : 'a Euler) =
        graph
            .SelectMany(fun kvp -> seq {for v in kvp.Value -> (kvp.Key, v)})
            .GroupBy(fun (o, i) -> i)
            .ToDictionary((fun gr -> gr.Key), (fun (gr : IGrouping<'a, 'a * 'a>) -> gr.Select(fun (o, i) -> o).ToList()))

(*deep copy a graph*)
let cloneDict (dct : 'a Euler) =
    dct.Select(fun kvp -> new KeyValuePair<'a, List<'a>>(kvp.Key, kvp.Value.Select(id).ToList())).ToDictionary((fun kvp -> kvp.Key), (fun (kvp : KeyValuePair<'a, List<'a>>) -> kvp.Value))

let  walk (gr : 'a Euler) =
    let start = gr.First().Key
    let mutable next = start
    let mutable count = 0
    let loop = List<'a>()
    while count = 0 || next <> start do
        loop.Add(next)
        next <- gr.[next].Single()
        count <- count + 1
    loop

let isConnected (gr : 'a Euler) =
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
                
    gr.Count = visited.Count

// compare two eulerian cycles
// cycles are represented as lists
let isEq (lstA : 'a List) (lstB : 'a List) =
    let fstElem = lstA.First()
    let idxA = lstB.IndexOf fstElem
    if idxA < 0 then false
    else
        let newTail = lstB.GetRange(0, idxA)
        lstB.RemoveRange(0, idxA)
        lstB.AddRange newTail
        lstB |> Seq.toList = (lstA |> Seq.toList)

let isPossibleLoop (gr : 'a Euler) =
    not (gr |> Seq.exists (fun kvp -> kvp.Value.Count > 1))

type 'a NewVertexGenerator = int -> int -> 'a -> 'a

// Get all eulerian cycles. First argument defines how a new vertex is generated.
let allEulerian<'a when 'a : equality> (newVertex: 'a NewVertexGenerator) (graph : 'a Euler) =
    seq{
        let allCycles = List<'a Euler * 'a Euler>()
        let allLoops = List<List<'a>>()
        let revGraph = reverseAdj graph

        allCycles.Add(graph, revGraph)

        let isDone () =
            allCycles.Count = 0 || (
                                    let curGraph, revCurGraph = allCycles.First()
                                    isPossibleLoop curGraph
            )
            
        while not (isDone ()) do
            let curGraph, revCurGraph = allCycles.First()
            let outVertices = revCurGraph.Where(fun kvp -> kvp.Value.Count > 1).Select(fun kvp -> kvp.Key)
            allCycles.RemoveAt(0)
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
                        if isConnected newGraph then
                            if isPossibleLoop newGraph then
                                let la = walk newGraph
                                if not (allLoops |> Seq.exists (fun e -> isEq e la)) then 
                                    allLoops.Add(la)
                                    yield la
                            else
                                allCycles.Add(newGraph, newRevGraph)               
    }

// replace all strings by their integer equivalents.
let convStringToIntGraph (graph : string Euler) =
    let fwdMap = graph.Keys |> fun sq -> sq.ToList()
    let bckwdMap = fwdMap |> Seq.mapi (fun i e -> (e, i)) |> fun sq -> sq.ToDictionary(fst, snd)
    let mapList (lst : string List) =
        lst |> Seq.map (fun e -> bckwdMap.[e]) |> fun s -> s.ToList()

    let intGraph = graph |> Seq.map (fun kvp -> bckwdMap.[kvp.Key], mapList kvp.Value) |> fun sq -> sq.ToDictionary(fst, snd)
    fwdMap, intGraph

// kinda bad. For our purposes, the new ineger vertex is generated from the current max one
// so the function has side effects...
let allEulerianInt (graph : string Euler) =
     
    let fwdMap, intGraph = convStringToIntGraph graph
    let curV = fwdMap.Count
    let newIntVertex _ _ v =
        let strV = fwdMap.[v]
        let vNext = fwdMap.Count + 1
        fwdMap.Add strV
        vNext

    (allEulerian newIntVertex intGraph), fwdMap

let graph = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, @"all_eulerian.txt")) |> parseStr