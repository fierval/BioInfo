open System
open System.Linq
open System.IO
#load @"..\Chapter 02 Molecular Clocks\environment.fsx"

let compl = dict [('A', 'T'); ('C', 'G'); ('G', 'C'); ('T', 'A')]
let revCompl (s : string) =
    string (String(s.Select(fun c -> compl.[c]).Reverse().ToArray()))

let complRev (s : string) = 
    string (String(s.Reverse() |> Seq.map (fun c -> compl.[c]) |> Seq.toArray))

let solve name =
    let s = File.ReadAllText name
    let sol = revCompl (s.Trim())
    File.WriteAllText(@"c:\temp\ch1_b.txt", sol)