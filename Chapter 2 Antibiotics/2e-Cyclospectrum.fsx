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

// array of amino acid weights
let weights = (aminoAcidOneLetterIntegerMassTrunc |> Seq.map (fun kvp -> kvp.Value)) |> Seq.toArray

let intSeqToRosalindWeights (weights : int seq) = 
    weights
        |> Seq.fold (fun state i -> state + (if String.IsNullOrEmpty state then "" else "-") + i.ToString()) String.Empty

let strToRosalindWeights (s : string) = 
    s.ToCharArray() 
        |> Array.map (fun c -> aminoAcidOneLetterIntegerMassTrunc.[c]) 
        |> intSeqToRosalindWeights

let toRosalind (outStr : List<string>) =
    outStr |> Seq.map strToRosalindWeights

let peptideFromLinearSpectrum (spec : int seq) =
    spec |> Seq.fold (fun state w -> state + massAminoAcid.[w].ToString()) String.Empty

let dictOfAminos (aminos : int seq) =
        aminos.GroupBy(fun a -> a).ToDictionary((fun gr -> gr.Key), 
            (fun (gr : IGrouping<int, int>) -> gr.Count()))

let isIn (dct : Dictionary<int, int>) (lst : int seq) =
    let tstDict = dictOfAminos lst
    let test = tstDict.TakeWhile(fun t -> dct.ContainsKey(t.Key) && dct.[t.Key] >= t.Value)
    test.Count() = tstDict.Count
        
/// spectrum must be an ordered array
let cyclopeptydeSequencing (spectrum : int []) =
    if spectrum = Unchecked.defaultof<int []> || spectrum.Length = 0 then failwith "Empty spectrum"
    
    let mass = spectrum.Last()
    let aminos = spectrum.[1..].TakeWhile(fun s -> s <= weights.Last()).ToArray()
    let expandWeights = aminos.Distinct().ToArray()
    let aminosDict = dictOfAminos aminos

    // create lists with an added peptide
    let expand (lst : List<List<int>>) =
        let newlst = List<List<int>>()
        for l in lst do
            for w in expandWeights do
                let newl = List<int>()
                newl.AddRange(l)
                newl.Add w
                if isIn aminosDict newl then
                    newlst.Add newl
        newlst

    // trim the list.
    // if mass of an element = mass of the spectrum - see if it qualifies for the output.
    // for the rest - make sure they are an exact subset of the spectrum
    let bound (lst : List<List<int>>) =
        let candidateOutput = lst.Where(fun l -> l.Sum() = mass)

        // trim the rest of the list, so each list is a subset of the spectrum
        let restLst = lst.Except(candidateOutput).ToList()

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
            printfn "%d" lst.Count
            let output, rest = bound lst
            out.AddRange output
            branchAndBound rest out
    
    let lst = List<List<int>>()
    Array.ForEach(expandWeights, (fun w -> lst.Add(List<int>()); lst.Last().Add w))
    let outStr = branchAndBound lst (List<string>())
    // this is due to the weird format Rosalind wants: 186-128-113 for instance
    toRosalind outStr |> Seq.fold (fun state s -> state + (if String.IsNullOrEmpty state then String.Empty else " ") + s) String.Empty
                   
    

let parseSpectrum (s : string) = s.Trim().Split(' ') |> Array.map int |> Array.sort

let solve name =
    let s = File.ReadAllText(name)
    let spectrum = parseSpectrum s
    let solution = cyclopeptydeSequencing spectrum
    File.WriteAllText(@"c:\temp\sol_cycle.txt", solution)

let spectrum = parseSpectrum "0 71 97 99 103 113 113 114 115 131 137 196 200 202 208 214 226 227 228 240 245 299 311 311 316 327 337 339 340 341 358 408 414 424 429 436 440 442 453 455 471 507 527 537 539 542 551 554 556 566 586 622 638 640 651 653 657 664 669 679 685 735 752 753 754 756 766 777 782 782 794 848 853 865 866 867 879 885 891 893 897 956 962 978 979 980 980 990 994 996 1022 1093"