open System
open System.Linq

let compl = dict [('A', 'T'); ('C', 'G'); ('G', 'C'); ('T', 'A')]
let revCompl (s : string) =
    s.ToUpper().Select(fun c -> compl.[c]).Reverse().Aggregate(String.Empty, (fun s c -> s + c.ToString()))
