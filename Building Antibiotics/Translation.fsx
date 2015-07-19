#load @"..\Lesson 2\environment.fsx"
#load "DataStructs.fsx"

open System.Linq
open System.Collections.Generic
open System
open System.IO
open DataStructs

let translate (rna : string) =
    let codones = toCodones rna
    let aminos = codones.Select(fun c -> aminoAcidOneLetter.[codonAminoAcid.[c]])

    aminos |> Seq.fold (fun st a -> st + a) String.Empty
            
let translateFile file =
    let rna = File.ReadAllText(file).Trim()
    File.WriteAllText(@"c:\temp\ant1.txt", translate rna)
