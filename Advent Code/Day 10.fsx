#load @"..\packages\FSharp.Charting.0.90.12\FSharp.Charting.fsx"

(*http://adventofcode.com/day/10 *)
open System.Linq
open System.Collections.Generic
open System.Diagnostics

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
    let sw = Stopwatch()
    sw.Start()
    let res =
        {1..n}
        |> Seq.fold 
            (fun st _ -> genNext st) (s |> Seq.map (fun s -> (string>>byte) s) |> fun a -> a.ToList())
        |> fun s -> s.Count
    sw.Stop()
    printfn "Elapsed: %A" sw.Elapsed
    res
        
let s = "1113122113"

(* F#-y way of solving it: http://theburningmonk.com/2015/12/advent-of-code-f-day-10/ *)
let read (input : string) =
    input
    |> Seq.fold (fun acc x ->
        match acc with
        | (n, x')::tl when x = x' -> (n+1, x')::tl
        | _ -> (1, x)::acc) []
    |> List.rev
    |> Seq.collect (fun (n, x) -> sprintf "%d%c" n x)
    |> fun xs -> System.String.Join("", xs)

let genseqf (s : string) n =
    let sw = Stopwatch()
    sw.Reset()
    sw.Start()
    let res = 
        { 1..n }
        |> Seq.fold (fun last _ -> read last) s
        |> Seq.length

    sw.Stop()
    printfn "Elapsed: %A" sw.Elapsed
    res