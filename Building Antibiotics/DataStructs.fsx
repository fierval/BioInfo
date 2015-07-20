open System
open System.Collections.Generic
open System.Linq
open System.IO

let alphabet = ['A'; 'C'; 'G'; 'U']

let inAlphabet = function
    | 'A' | 'C' | 'U' | 'G' -> true
    |_ -> false

let toRna (dna : char []) =
    dna |> Array.map (fun c -> if c <> 'T' then c else 'U')

let strInAlphabet (letters : char []) = (letters  |> Seq.filter inAlphabet |> Seq.length) = letters.Length


let getDictFromCsv csv =
    let lines = 
        File.ReadAllLines(csv) 
        |> Array.map(fun l -> l.Split(',') |> Array.map(fun s -> s.Trim()))
    lines.ToDictionary((fun l -> l.[0]), (fun (l : string []) -> l.[1]))

let codonAminoAcid = getDictFromCsv (Path.Combine(__SOURCE_DIRECTORY__, "CodonAminoAcid.csv"))
let aminoAcidOneLetter = getDictFromCsv (Path.Combine(__SOURCE_DIRECTORY__, "AminoAcids.csv"))

let toCodones (rna : string) =
    let codones : string [] = Array.zeroCreate (rna.Length / 3)
    for i = 0 to codones.Length * 3 - 1 do
        codones.[i/3] <- codones.[i/3] + rna.[i].ToString()
    codones
