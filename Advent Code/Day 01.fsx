(*http://adventofcode.com/*)
open System.IO

let s = File.ReadAllText(@"c:\users\boris\downloads\input.txt")

s |> Seq.fold (fun state c -> if c = '(' then state + 1 else state - 1) 0

s |> Seq.scan (fun state c -> if c = '(' then state + 1 else state - 1) 0 |> Seq.skip 1  |> Seq.findIndex (fun e -> e = -1) |> (+) 1