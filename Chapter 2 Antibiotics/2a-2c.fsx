#load @"..\Chapter 3 Molecular Clocks\environment.fsx"
#load "DataStructs.fsx"
#load @"..\Chapter 1 DnaA Box\reverseCompl.fsx"

open System.Linq
open System.Collections.Generic
open System
open System.IO
open DataStructs

let toRna (dna : string) =
    let dnaFiltered = dna.ToCharArray() |> Array.filter inDnaAlphabet
    if dnaFiltered.Length <> dna.Length then failwith "bad alphabet"
    dna |> String.map (fun c -> if c = 'T' then 'U' else c)

let toDna (rna : string) =
    rna |> String.map (fun c -> if c = 'U' then 'T' else c)

// not sure what to do with the "stop" nucleotide, so...
let translateKeepStop keepStop (rna : string)  =
    let codones = toCodones rna
    let aminos = codones.Select(fun c -> aminoAcidOneLetter.[codonAminoAcid.[c]])
    let res = if keepStop then aminos.TakeWhile(fun c -> c <> "X") else aminos

    res |> Seq.fold (fun st a -> st + a) String.Empty

let translate = translateKeepStop true
            
let translateFile file =
    let rna = File.ReadAllText(file).Trim()
    File.WriteAllText(@"c:\temp\ant1.txt", translate rna)

let findEncodingSubstr (dna : string) (peptide : string) =
    let rna = toRna dna
    let revComplDna = ReverseCompl.revCompl dna
    let revRna = toRna revComplDna

    let findSubstrs (rna : string) =
        // there are three reading frames per strand
        seq {
            for i = 0 to 2 do
                let strRna = rna.Substring(i)
                for idx in i..3..strRna.Length - 3 * peptide.Length do
                    let sub = strRna.[idx..idx + 3 * peptide.Length - 1]
                    let subPeptide = translate sub
                    if subPeptide = peptide then yield sub
        }

    let straights = findSubstrs rna |> Seq.map toDna
    let revs = findSubstrs revRna |> Seq.map toDna |> Seq.map ReverseCompl.complRev

    [|yield! straights; yield! revs|]

let findEncodingSubstrFile file =
    let outf = @"c:\temp\ant2.txt"
    let lines = File.ReadAllLines(file)
    let dna = lines.[0].Trim()
    let peptide = lines.[1].Trim()
    let seqs = findEncodingSubstr dna peptide |> Seq.toArray
    File.WriteAllLines(outf, seqs)

let cyclopsecArr (masses : int []) = 
    let generateSpectrum i =
        seq {
            for j = 0 to masses.Length - i - 1 do
                yield (Array.sum masses.[j..j+i])
        
            // cyclical
            for j = masses.Length - i to masses.Length - 1 do
                let headpart = masses.[j..]
                let headsum = (Array.sum headpart)
                let tailsum = (Array.sum masses.[0..i-headpart.Length])
                yield tailsum + headsum
        }    
    let subspec = [0..masses.Length - 2] |> Seq.collect generateSpectrum |> Seq.sort
    seq {yield 0; yield! subspec; yield Array.sum masses}

// generate cyclospectrum from a peptide
let cyclospectrum (peptide : string) =
    let masses = 
        peptide.ToCharArray() 
        |> Array.map (fun c -> aminoAcidOneLetterIntegerMass.[c])

    let generateSpectrum i =
        seq {
            for j = 0 to masses.Length - i - 1 do
                yield (Array.sum masses.[j..j+i])
        
            // cyclical
            for j = masses.Length - i to masses.Length - 1 do
                let headpart = masses.[j..]
                let headsum = (Array.sum headpart)
                let tailsum = (Array.sum masses.[0..i-headpart.Length])
                yield tailsum + headsum
        }    
    let subspec = [0..peptide.Length - 2] |> Seq.collect generateSpectrum |> Seq.sort
    seq {yield 0; yield! subspec; yield Array.sum masses}

let solveCyc (peptide : string) =
    let solution = cyclospectrum peptide
    let txt = solution |> Seq.fold (fun state e -> if String.IsNullOrEmpty state then e.ToString() else state + " " + e.ToString()) String.Empty
    File.WriteAllText(@"c:\temp\ant3.txt", txt)


// for debugging. Generate sub-arrays of characters from a string
let generateSpectrum (peptide : string) i =
    let masses = peptide.ToCharArray()
    seq {
        for j = 0 to masses.Length - i - 1 do
            yield String(masses.[j..j+i])
        
        // cyclical
        for j = masses.Length - i to masses.Length - 1 do
            let headpart = masses.[j..]
            let headsum = headpart
            let tailsum = masses.[0..i-headpart.Length]
            yield String(headsum) + String(tailsum)
    }
