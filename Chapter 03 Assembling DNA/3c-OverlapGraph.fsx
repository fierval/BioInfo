open System.Linq
open System.IO

#load @"..\Chapter 02 Molecular Clocks\environment.fsx"

let suffix (pat : string) = pat.[1..]
let prefix (pat : string) = pat.[0..pat.Length - 2]
let isConnected p1 p2 = (suffix p1) = (prefix p2)

let overlapGraph (kmers : string []) =
    kmers
        .SelectMany
            (fun k -> 
                let selected = kmers.Where(fun k2 -> isConnected k k2)
                selected.Select(fun s -> k, s).AsEnumerable())

let decorate (graph : seq<string * string>) = graph.OrderBy(fun (from, to') ->from).ThenBy(fun (from, to') -> to').Select (fun (from, to') -> from + " -> " + to')

let kmers = [|"ATGCG";"GCATG"; "CATGC"; "AGGCA"; "GGCAT"|]

let ar = (overlapGraph >> decorate) kmers |> Seq.toArray


let solve name = 
    let lines = File.ReadAllLines(name)
    let kmers = lines |> Array.map (fun e -> e.Trim([|' '; '\t'; '\n'; '\r'|]))
    let ar = (overlapGraph >> decorate) kmers |> Seq.toArray
    File.WriteAllLines(@"c:\temp\graph.txt", ar)

let name = "overlap_graph.txt"
    