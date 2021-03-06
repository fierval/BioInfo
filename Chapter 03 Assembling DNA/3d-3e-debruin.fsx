﻿open System.Linq
open System.IO
open System.Collections.Generic

#load @"3a-3b.fsx"
#load @"3c-OverlapGraph.fsx"
#load @"3f-EulerCycle.fsx"

open ``3a-3b``
open ``3c-OverlapGraph``
open ``3f-EulerCycle``

let debruijnString (s : string) n =
    let kmers = decompose s (n - 1)

    let pathGraph =
        seq {
            for i = 0 to kmers.Length - 2 do
                yield (kmers.[i], kmers.[i + 1])
        }

    pathGraph.ToLookup(fst, snd)
        .ToDictionary((fun gr-> gr.Key), (fun (gr : IGrouping<string, string>) -> gr.OrderBy(fun e -> e).ToList()))

let decorate (graph : Dictionary<string, List<string>>) =
    graph
        .Select(fun kvp -> kvp.Key + " -> " + kvp.Value.Aggregate(fun st s -> st + "," + s)).ToArray()

let s = "AAGATTCTCTAC"
let n = 4

let solveStr name =
    let lines = File.ReadAllLines(name)
    let n = int (lines.[0].Trim())
    let s = lines.[1].Trim()
    let sol = debruijnString s n |> decorate
    File.WriteAllLines(@"c:\temp\debruin.txt", sol)

let debruijnBase (pref : 'a -> 'a) (suff : 'a -> 'a) (kmers : 'a seq) : 'a Euler = 
    kmers.ToLookup(pref, suff)
        .ToDictionary((fun gr-> gr.Key), (fun (gr : IGrouping<'a, 'a>) -> gr.OrderBy(fun e -> e).ToList()))

let debruijnPaired : seq<string * string> -> Euler<string * string> = 
    debruijnBase (fun (f, s) -> prefix f, prefix s) (fun (f, s) -> suffix f, suffix s)

let debruijn : seq<string> -> Euler<string> = debruijnBase prefix suffix

let kmers = [|"GAGG"; "CAGG"; "GGGG"; "GGGA"; "CAGG"; "AGGG"; "GGAG"|]

let solve name = 
    let kmers = File.ReadAllLines(name) |> Array.map(fun s -> s.Trim())
    let sol = (debruijn >> decorate) kmers
    File.WriteAllLines(@"c:\temp\debruijn.txt", sol)