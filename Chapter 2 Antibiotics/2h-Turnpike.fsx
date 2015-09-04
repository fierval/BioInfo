#load "2e-Cyclospectrum.fsx"

open System
open System.Linq
open System.Collections.Generic
open ``2e-Cyclospectrum``
open System.IO

Environment.CurrentDirectory <- @"c:\users\boris\downloads"

type DecisionTree =
    | Empty
    | TreeNode of deltas : Dictionary<int, int> * solution : int list * visited: uint32 * maxElem : DecisionTree * maxDist : DecisionTree * prev : DecisionTree 

let keySeqMax (ds : Dictionary<int, int>) = ds.Keys |> Seq.max

let removeDist (deltas : Dictionary<int, int>) (distances : int seq) =
    let nd = deltas 
            |> Seq.fold(
                fun (state : Dictionary<int, int>) dst -> 
                    state.Add(dst.Key, dst.Value); state) (Dictionary<int, int>())

    for d in distances do
      if nd.[d] = 1 then nd.Remove(d) |> ignore
      else
        nd.[d] <- nd.[d] - 1
    nd

let stepOk elem (res : int list) deltas =
    if res.Length = 0 then [elem]
    else
        let distances = res |> List.map (fun r -> abs (elem - r))
        if isIn deltas distances then distances else []

let visit =
    function
    | Empty -> Empty
    | TreeNode(deltas, solution, visited, maxElem, maxDist, prev) -> 
        TreeNode(deltas, solution, visited + 1u, maxElem, maxDist, prev)

let getPrev =
    function
    | Empty -> [], Unchecked.defaultof<Dictionary<int, int>>, Empty
    | TreeNode (deltas, res, visited, maxElem, maxDist, prev) -> res, deltas, prev

let rec insert (deltas : Dictionary<int, int>) (res : int list) (node : DecisionTree) (prevNode : DecisionTree) maxSol =
    
    let insPrev () =
        let res, deltas, prevPrev = getPrev prevNode
        insert deltas res prevNode prevPrev maxSol

    match node with 
    | Empty -> TreeNode(deltas, res, 0u, Empty, Empty, prevNode)
    | TreeNode(dct, rslt, visited, maxElem, maxDist, prev) when visited < 2u ->
        let curVisited = visit node
        let elem = 
            if visited = 0u then keySeqMax deltas else maxSol - keySeqMax deltas
        let dists = stepOk elem res deltas
        if dists.Length > 0 then
            let newDeltas = removeDist deltas dists
            insert newDeltas (elem::res) (if visited = 0u then maxElem else maxDist) curVisited maxSol
        elif visited = 0u then
            insert deltas res curVisited prev maxSol
        else
            insPrev()
    | _ -> insPrev()

let turnpike (dA : int seq) =
    //hashset of ditance -> # times appearing
    let deltas = dA |> Seq.filter (fun d -> d > 0) |> dictOfAminos 
    let maxSol = keySeqMax deltas

    let rec buildSolution (deltas : Dictionary<int, int>) (res : int list) (node : DecisionTree) (prev : DecisionTree) =
        if deltas.Count = 0 then res |> List.sort
        else
            let newNode = insert deltas res node prev maxSol
            let prevNode = node
            match newNode with
            | Empty -> [] // no solution
            | TreeNode(deltas, res, visited, maxElem, maxDist, prev) ->
                if visited >=2u && prev = Empty then [] // came all the way back, no solution
                else
                    buildSolution deltas res newNode prevNode

    // validate that the length of the diffs set contains just the right number of entries
    let origLength = deltas.Values |> Seq.sum
    let solLength = int (ceil(1. + sqrt(1. + (8. * float origLength)))/ 2.)

    if (solLength - 1) * solLength / 2 <> origLength then failwith "Incorrect array of diffs"
    else
        buildSolution deltas [0] Empty Empty

let generateDeltas (sol : int seq) =
    sol
    |> Seq.map (fun s -> sol |> Seq.map (fun s1 -> s1 - s)) |> Seq.collect (fun s -> s) |> Seq.toList |> List.sort
    
let dA = generateDeltas [0; 2; 4; 7; 10]
let res = [0]
let node, prev = Empty, Empty

let solve name =
    let lines = File.ReadAllText(name)
    let dA = lines.Split() |> Seq.map(fun s -> int s)
    let sol = turnpike dA |> Seq.toArray
    let solstr = sol |> Seq.fold(fun state s -> state + " " + string s) String.Empty
    File.WriteAllText(@"c:\temp\turnpike.txt", solstr.TrimStart())