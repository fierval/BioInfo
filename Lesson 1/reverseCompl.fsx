open System
open System.Linq

let compl = dict [('A', 'T'); ('C', 'G'); ('G', 'C'); ('T', 'A')]
let revCompl (s : string) =
    string (String(s.Select(fun c -> compl.[c]).Reverse().ToArray()))

let complRev (s : string) = 
    string (String(s.Reverse() |> Seq.map (fun c -> compl.[c]) |> Seq.toArray))
