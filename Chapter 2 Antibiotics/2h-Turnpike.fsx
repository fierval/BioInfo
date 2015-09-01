#load "2e-Cyclospectrum.fsx"

open System
open System.Linq
open System.Collections.Generic
open ``2e-Cyclospectrum``

type Decision =
    | MaxElem
    | MaxDistance
    | Revert

type DecisionTree =
    | Empty
    | MaxElem of Dictionary<int, int> * int list * DecisionTree * DecisionTree * DecisionTree
    | MaxDist of Dictionary<int, int> * int list * DecisionTree * DecisionTree * DecisionTree

let nextState (state : Decision) =
    function
    | MaxElem -> MaxDistance
    | MaxDistance -> Revert
    | Revert -> MaxElem

let turnpike (dA : int seq) =
    //hashset of ditance -> # times appearing
    let deltas = dA |> Seq.filter (fun d -> d > 0) |> dictOfAminos
    deltas.Add(0, 1)
    let keySeqMax ds = ds.Keys |> Seq.max
    let maxSol = keySeqMax deltas
    let deltaLength (dct : IDictionary<'T, 'U>) = dct.Values |> Seq.sum
    let origLength = deltaLength deltas

    let isNonEmptyDict (dct : Dictionary<'T, 'U>) = dct <> Unchecked.defaultof<Dictionary<int, int>>
    let removeDist (deltas : Dictionary<int, int>) (distanes : int seq) =
        let nd = deltas |> Seq.fold(fun state dst -> state.Add(dst.Key, dst.Value); state) (Dictionary<int, int>())
            for d in distances do
              if nd.[d] = 1 then nd.Remove(d)
              else
                nd.[d] <- nd.[d] - 1
            nd

    let stepOk elem res deltas =
        let distances = (elem :: res) |> Seq.map (fun r -> abs (elem - r)) |> Seq.toList
        if isIn deltas distances then distances else []

    let insert (deltas : Dictionary<int, int>) (res : int list) (node : DecisionTree) =

        match node with 
        | Empty -> MaxElem(deltas, res, Empty, Empty, Empty)
        | MaxElem(dct, rslt, maxElem, maxDist, prev) as cur ->
            if maxElem = Empty then
                MaxElem(deltas, res, Empty, Empty, cur)
    

    let withBacktrack (deltas : Dictionary<int, int>) (res : int list) (treeState : Decision) =

        if deltas.Count = 0 then res
        elif res = [-1] then
            let treeState = nextState treeState
            if deltaLength deltas = origLength && treeState = Revert then res // no solution
            else           
                withBacktrack deltas res (nextState treeState)  
        else
            let elem = 
                match treeState with
                | MaxElem -> keySeqMax deltas
                | MaxDistance -> maxSol - (keySeqMax deltas)
                | Revert -> -1

            let nd = stepOk elem
            if isNonEmptyDict then
                withBacktrack nd (elem :: res) treeState
            elif treeState = MaxElem then 
                withBacktrack deltas res MaxDistance
            else
                [-1] // we need to completely revert everything
                