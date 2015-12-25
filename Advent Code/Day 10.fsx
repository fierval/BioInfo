(*http://adventofcode.com/day/10 *)
open System.Linq
open System.Collections.Generic

let genNext (s : List<byte>) =
    let mutable i = 0y
    let mutable c = s.[0]
    let reps = List<byte>()
    for ch in s do
        if c <> ch then
            reps.Add(byte i)
            reps.Add(c)
            c <- ch
            i <- 0y
        i <- i + 1y
    reps.Add(byte i)
    reps.Add(c)
    reps

let genseq (s : string) n =
    {1..n}
    |> Seq.fold 
        (fun st _ -> genNext st) (s |> Seq.map (fun s -> (string>>byte) s) |> fun a -> a.ToList())
    |> fun s -> s.Count

let s = "1113122113"