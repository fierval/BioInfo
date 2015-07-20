#load @"..\Lesson 2\environment.fsx"
#load "DataStructs.fsx"
#load @"..\Lesson 1\reverseCompl.fsx"

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

let translate (rna : string) =
    let codones = toCodones rna
    let aminos = codones.Select(fun c -> aminoAcidOneLetter.[codonAminoAcid.[c]]).TakeWhile(fun c -> c <> "X")

    aminos |> Seq.fold (fun st a -> st + a) String.Empty
            
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
                for idx = 0 to strRna.Length - peptide.Length - 1 do
                    let sub = strRna.[idx..idx + peptide.Length - 1]
                    let subPeptide = translate sub
                    if subPeptide = peptide then yield sub
        }

    let straights = findSubstrs rna |> Seq.map toDna
    let revs = findSubstrs revComplDna |> Seq.map toDna |> Seq.map ReverseCompl.complRev

    [|yield! straights; yield! revs|]
