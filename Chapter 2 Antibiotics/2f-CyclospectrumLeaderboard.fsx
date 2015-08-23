open System
open System.Linq
open System.Collections.Generic
open System.IO

#load "2e-Cyclospectrum.fsx"

open ``2e-Cyclospectrum``
open ``2a-2c``

/// spectrum must be an ordered array, n - top n entries on the leaderboard with ties
let cyclopeptydeLeaderboard (spectrum : int []) n =
    if spectrum = Unchecked.defaultof<int []> || spectrum.Length = 0 then failwith "Empty spectrum"
    let mass = spectrum.Last()
    let leaderPeptide = List<int>()
    leaderPeptide.Add(weights.[0])

    // convert spectrum to the dictionary of mass -> #times occuring
    let specDict = dictOfAminos spectrum

    /// number of peptides shared between cyclospectrums of current peptide and the given spectrum
    let score (peptide : int seq)  =
        let cyclospec = (peptideFromLinearSpectrum >> cyclospectrum) peptide
        let cycloDict = dictOfAminos cyclospec
        let keys = specDict.Keys.Intersect(cyclospec)
        keys 
        |> Seq.map (fun k -> if cycloDict.[k] <= specDict.[k] then cycloDict.[k] else specDict.[k])
        |> Seq.sum

    // cut the leaderboard to have only n items with ties
    let cut (board : List<List<int>>) n =
        board
            .GroupBy(fun e -> score e)
            .OrderByDescending(fun gr -> gr.Key)
            .Take(n)
            .SelectMany(fun gr -> gr :> IEnumerable<List<int>>)
            .ToList()


    // create lists with an added peptide
    let expand (lst : List<List<int>>) =
        let newlst = List<List<int>>()
        for l in lst do
            for w in weights do
                let newl = l.Clone()
                newl.Add w
                newlst.Add newl
        newlst

    // trim the list.
    // if mass of an element = mass of the spectrum - see if it qualifies for the output.
    // for the rest - make sure they are an exact subset of the spectrum
    let bound (lst : List<List<int>>) =

        // determine the new leader
        let candidateOutput = lst |> Seq.filter (fun l -> l |> Seq.sum = mass)
        if candidateOutput.Any() then
            let candidateLeader = candidateOutput |> Seq.maxBy (fun s -> score s)
            if score candidateLeader > score leaderPeptide then
                leaderPeptide.Clear()
                leaderPeptide.AddRange(candidateLeader)

        // trim the rest of the list, so each list is a subset of the spectrum
        let toRemove = lst.Where(fun l -> l.Sum() > mass)
        let rest = lst.Except(toRemove).ToList()
        if rest.Any() then
            cut rest n
        else
            rest
        
    let rec branchAndBound (lst : List<List<int>>) =
        if lst.Count = 0 then leaderPeptide
        else
            let lst = expand lst
            printfn "%d" lst.Count
            let rest = bound lst
            branchAndBound rest
    
    let leaderboard = List<List<int>>()
    Array.ForEach(weights, (fun w -> leaderboard.Add(List<int>()); leaderboard.Last().Add w))
    let leaderboard = cut leaderboard n

    let outStr = branchAndBound leaderboard
    
    // this is due to the weird format Rosalind wants: 186-128-113 for instance
    intSeqToRosalindWeights outStr

let spectrum = parseSpectrum "0 71 113 129 147 200 218 260 313 331 347 389 460"
let n = 10