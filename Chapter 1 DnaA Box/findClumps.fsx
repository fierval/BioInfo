#load "countKmers.fsx"
#load @"..\Chapter 3 Molecular Clocks\environment.fsx"

open System.Linq
open System.Collections.Generic
open System.IO
open CountKmers

/// <summary>
/// Find (L, t) clumps in a genome s
/// </summary>
/// <param name="s">genome string</param>
/// <param name="k">length of k-mer</param>
/// <param name="t">exact number of occurences of the k-mer</param>
let findClumps (s : string) k L t =
    let kmers = HashSet<string>()
    for substr in [0..s.Length - L].Select(fun i -> s.Substring(i, L)) do
        let dic = kMers substr k
        let filtered = dic.Where(fun kvp -> kvp.Value = t).ToList()
        if filtered.Count > 0 then
            filtered.ForEach(fun e -> (kmers.Add e.Key) |> ignore)

    kmers.ToArray()


let solve name =
    let lines = File.ReadAllLines name
    let params' = lines.[1].Split(' ').Select(fun s -> int s).ToArray()
    let k, L, t = params'.[0], params'.[1], params'.[2]
    let out = findClumps (lines.[0]) k L t
    File.WriteAllLines(@"c:\temp\sol3.txt", out)
    
           