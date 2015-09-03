#load "2e-Cyclospectrum.fsx"

open System
open System.Linq
open System.Collections.Generic
open ``2e-Cyclospectrum``

type DecisionTree =
    | Empty
    | TreeNode of deltas : Dictionary<int, int> * solution : int list * maxElem : DecisionTree * maxDist : DecisionTree * prev : DecisionTree 

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

let stepOk elem res deltas =
    let distances = (elem :: res) |> Seq.map (fun r -> abs (elem - r)) |> Seq.toList
    if isIn deltas distances then distances else []

let isEmpty = 
    function
    | Empty -> true
    | _ -> false

let getPrev =
    function
    | Empty -> Empty
    | TreeNode (deltas, res, maxElem, maxDist, prev) -> prev

let rec insert (deltas : Dictionary<int, int>) (res : int list) (node : DecisionTree) (prevNode : DecisionTree) maxSol =

    match node with 
    | Empty -> TreeNode(deltas, res, Empty, Empty, prevNode)
    | TreeNode(dct, rslt, maxElem, maxDist, prev) as cur ->
        if isEmpty maxElem || isEmpty maxDist then
            let elem = if isEmpty maxElem then keySeqMax deltas else maxSol - keySeqMax deltas
            let dists = stepOk elem res deltas
            if dists.Length > 0 then
                let newDeltas = removeDist deltas dists
                if isEmpty maxElem then
                    insert newDeltas (elem::res) maxElem cur maxSol
                else 
                    insert newDeltas (elem::res) maxDist cur maxSol
            else
                insert deltas res prev (getPrev prev) maxSol
        else
            insert deltas res prev (getPrev prev) maxSol

let turnpike (dA : int seq) =
    //hashset of ditance -> # times appearing
    let deltas = dA |> Seq.filter (fun d -> d > 0) |> dictOfAminos
    deltas.Add(0, 1)
    let maxSol = keySeqMax deltas

    let rec buildSolution (deltas : Dictionary<int, int>) (res : int list) (node : DecisionTree) (prev : DecisionTree) =
        if deltas.Count = 0 then res
        else
            let newNode = insert deltas res node prev maxSol
            let prevNode = node
            match newNode with
            | Empty -> [] // no solution
            | TreeNode(deltas, res, maxElem, maxDist, prev) ->
                if not (isEmpty maxElem) && not (isEmpty maxDist) && isEmpty prev then [] // came all the way back, no solution
                else
                    buildSolution deltas res newNode prevNode

    // validate that the length of the diffs set contains just the right number of entries
    let origLength = deltas.Values |> Seq.sum
    let solLength = int (floor(1. + sqrt(1. + (8. * float origLength)))/ 2.)
    if (solLength - 1) * solLength / 2 <> origLength then []
    else
        buildSolution deltas [0] Empty Empty

    