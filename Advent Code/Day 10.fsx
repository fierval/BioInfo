open System.IO
open System
open System.Linq
open System.Collections.Generic
open System.Diagnostics
open System.Text

let name = @"c:\users\boris\downloads\input.txt"

let genNext (s : List<byte>) =
    let mutable stop = false        
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

let genList (st : string) =
    st.ToCharArray().Select(fun e -> byte (e.ToString())).ToList() 

let genseq (s : string) n =
    let sw = Stopwatch()
    sw.Start()
    let stl = genList s
    let str = 
        stl
        |> Seq.unfold (fun st -> Some(st, genNext st))
        |> Seq.skip n
        |> Seq.take 1
    let res = str.Single().Count
    sw.Stop()
    printfn "Elapsed time: %A" sw.Elapsed
    res

let s = "1113122113"