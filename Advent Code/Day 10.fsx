#load @"..\packages\FSharp.Charting.0.90.12\FSharp.Charting.fsx"
open FSharp.Charting

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
    let res =
        {1..n}
        |> Seq.fold 
            (fun st _ -> genNext st) (s |> Seq.map (fun s -> (string>>byte) s) |> fun a -> a.ToList())
        |> fun s -> s.Count
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
    let res = 
        { 1..n }
        |> Seq.fold (fun last _ -> read last) s
        |> Seq.length

    res

let compare (s : string ) (basis : int list) =
    let sw = Stopwatch()
    let timeIt e (f : string -> int -> int) = 
        {1..3} 
            |> Seq.map (fun _ -> 
                        sw.Reset()
                        sw.Start()
                        f s e |> ignore
                        float sw.ElapsedMilliseconds
                        )
            |> Seq.average
    
    let cpp = basis |> Seq.fold (fun st e -> [e, timeIt e genseq] @ st) List.empty
    let fs = basis |> Seq.fold (fun st e -> [e, timeIt e genseqf] @ st) List.empty

    Chart.Combine(
        [Chart.Line(cpp, Name="CPP-like")
         Chart.Line(fs, Name="FSharpy")
        ] 
    )
        .WithYAxis(Log=false, Title = "msec")
        .WithXAxis(Title = "times")
        .WithLegend(InsideArea=false) 

//compare s [40..3..51]