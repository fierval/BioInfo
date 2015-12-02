open System.Linq
open System.IO
open System.Collections.Generic

#load @"3a-3b.fsx"
#load @"3c-OverlapGraph.fsx"

open ``3a-3b``
open ``3c-OverlapGraph``

let debruijnString (s : string) n =
    let kmers = decompose s (n - 1)

    let pathGraph =
        seq {
            for i = 0 to kmers.Length - 2 do
                yield (kmers.[i], kmers.[i + 1])
        }

    pathGraph.ToLookup(fst, snd)
        .OrderBy(fun kvp -> kvp.Key)
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

let debruijn (kmers : string []) =
    kmers.ToLookup(prefix, suffix)
        .OrderBy(fun kvp -> kvp.Key)
        .ToDictionary((fun gr-> gr.Key), (fun (gr : IGrouping<string, string>) -> gr.OrderBy(fun e -> e).ToList()))

let kmers = [|"GAGG"; "CAGG"; "GGGG"; "GGGA"; "CAGG"; "AGGG"; "GGAG"|]

let solve name = 
    let kmers = File.ReadAllLines(name) |> Array.map(fun s -> s.Trim())
    let sol = (debruijn >> decorate) kmers
    File.WriteAllLines(@"c:\temp\debruijn.txt", sol)