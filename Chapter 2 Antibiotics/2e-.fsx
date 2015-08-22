open System
open System.Linq
open System.Collections.Generic
open System.IO
open System.Runtime.CompilerServices

#load @"..\Chapter 3 Molecular Clocks\environment.fsx"
#load "DataStructs.fsx"
#load "2a-2c.fsx"

open DataStructs
open ``2a-2c``

[<Extension>]
type ExtensionsForList () =
    [<Extension>]
    static member inline Clone (lst : List<'T>) = lst.Select(fun l -> l).ToList()

// array of amino acid weights
let weights = (aminoAcidOneLetterIntegerMassTrunc |> Seq.map (fun kvp -> kvp.Value)) |> Seq.toArray

let peptideFromLinearSpectrum (spec : int seq) =
    spec |> Seq.fold (fun state w -> state + massAminoAcid.[w].ToString()) String.Empty

/// spectrum must be an ordered array
let cyclopeptydeSequencing (spectrum : int []) =
    if spectrum = Unchecked.defaultof<int []> || spectrum.Length = 0 then failwith "Empty spectrum"
    
    let mass = spectrum.Last()

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
        let candidateOutput = lst.Where(fun l -> l.Sum() = mass)

        // trim the rest of the list, so each list is a subset of the spectrum
        let restLst = lst.Except(candidateOutput).Where(fun l -> l.Intersect(spectrum).Count() = l.Count).ToList()

        (seq {
            for l in candidateOutput do
                let pep = peptideFromLinearSpectrum l
                let cyclospec = cyclospectrum pep |> Seq.toArray
                if cyclospec = spectrum then 
                    yield pep
        }).ToList(), restLst
        
    let rec branchAndBound (lst : List<List<int>>) (out : List<string>) =
        if lst.Count = 0 then out
        else
            let lst = expand lst
            let output, rest = bound lst
            out.AddRange output
            branchAndBound lst out
    
    let lst = List<List<int>>()
    Array.ForEach(weights, (fun w -> lst.Add(List<int>()); lst.Last().Add w))
    let outStr = branchAndBound lst (List<string>())
    // this is due to the weird format Rosalind wants: 186-128-113 for instance
    outStr 
    |> Seq.map 
        (fun s -> s.ToCharArray() 
                    |> Array.map (fun c -> aminoAcidOneLetterIntegerMassTrunc.[c]) 
                    |> Array.fold (fun state i -> state + "-" + i.ToString()) String.Empty)

