#load "2e-Cyclospectrum.fsx"

open System
open System.Linq
open System.Collections.Generic
open ``2e-Cyclospectrum``

type DecisionTree =
    | Empty
    | TreeNode of deltas : Dictionary<int, int> * solution : int list * maxElem : DecisionTree * maxDist : DecisionTree * prev : DecisionTree * visited : int

let isNonEmptyDict (dct : Dictionary<int, int>) = dct <> Unchecked.defaultof<Dictionary<int, int>>

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

let visit =
    function
    | Empty -> Empty
    | TreeNode(dct, sol, maxElem, maxDist, prev, visited) -> 
        TreeNode(dct, sol, maxElem, maxDist, prev, visited + 1)

let rec insert (deltas : Dictionary<int, int>) (res : int list) (node : DecisionTree) maxSol =

    match node with 
    | Empty -> TreeNode(deltas, res, Empty, Empty, Empty, 0)
    | TreeNode(dct, rslt, maxElem, maxDist, prev, visited) as cur ->
        if visited < 2 then
            let elem = if visited = 0 then keySeqMax deltas else maxSol - keySeqMax deltas
            let dists = stepOk elem res deltas
            if dists.Length > 0 then
                let newDists = removeDist deltas dists
                if visited = 0 then
                    insert deltas (elem::res) maxElem maxSol
                else 
                    insert deltas (elem::res) maxDist maxSol
            else
                let visitedTree = visit cur
                insert deltas res visitedTree maxSol
        else
            insert deltas res prev maxSol

let turnpike (dA : int seq) =
    //hashset of ditance -> # times appearing
    let deltas = dA |> Seq.filter (fun d -> d > 0) |> dictOfAminos
    deltas.Add(0, 1)
    let maxSol = keySeqMax deltas
    let deltaLength (dct : IDictionary<'T, 'U>) = dct.Values |> Seq.sum
    let origLength = deltaLength deltas

    let buildSolution (deltas : Dictionary<int, int>) (res : int list) =
        if deltas.Count()

    

    