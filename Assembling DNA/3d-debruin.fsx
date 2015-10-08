open System.Linq
open System.Collections.Generic

#load @"3a-3b.fsx"
#load @"3c-OverlapGraph.fsx"

open ``3a-3b``
open ``3c-OverlapGraph``

let debruijn (s : string) n =
    let kmers = decompose s (n - 1)

    let pathGraph =
        seq {
            for i = 0 to kmers.Length - 2 do
                yield (kmers.[i], kmers.[i + 1])
        }

    pathGraph.ToLookup((fun (p1, p2) -> p1),((fun (p1, p2) -> p2)))

let decorate (graph : ILookup<string, string>) =
    graph.OrderBy(fun kvp -> kvp.Key).ToDictionary((fun gr-> gr.Key), (fun (gr : IGrouping<string, string>) -> gr.OrderBy(fun e -> e).ToList()))

let s = "AAGATTCTCTAC"
let n = 4
