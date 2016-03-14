open System
open System.Linq
open System.IO
open System.Collections.Generic

/// <summary>
/// Hamming distance between two strings
/// </summary>
/// <param name="s1"></param>
/// <param name="s2"></param>
let hammingDist (s1 : string) (s2 : string) =
    s1.Zip(s2, (fun e1 e2 -> (e1, e2))).Where(fun (a, b) -> a <> b).Count()

Environment.CurrentDirectory <- @"C:\users\boris\Downloads"
let hdFile path =
    let lines = File.ReadLines(path).Select(fun s -> s.Trim()).ToArray()
    hammingDist lines.[0] lines.[1]

/// <summary>
/// Find all starting positions of occurences of a pattern such that
/// Hamming(substr, pat) <= d
/// </summary>
/// <param name="s"></param>
/// <param name="d"></param>
let withMisMatch (s : string) (pat : string) d =
    let pos = 
        0 
        |> Seq.unfold (fun i -> 
                        if i > s.Length - pat.Length then 
                            None 
                        else 
                            Some((if (hammingDist pat (s.Substring(i, pat.Length))) <= d then i else -1), (i+1)))
    pos |> Seq.where(fun p -> p >= 0) |> Seq.toArray

/// <summary>
/// Count all occurences of pat in s where Hamming(substr, pat) <= d
/// </summary>
/// <param name="s"></param>
/// <param name="pat"></param>
/// <param name="d"></param>
let countD (s : string) (pat : string) d =
    let pos = withMisMatch s pat d
    pos.Count(fun p -> p >= 0)

/// <summary>
/// k-mers within one string and their counts, with a Hamming distance <= d
/// </summary>
/// <param name="s"></param>
/// <param name="k"></param>
let kMersMutat (s : string) k d =
    let kmers = [0..s.Length - k].Select(fun i -> s.Substring(i, k))
    let dic = HashSet(kmers).ToDictionary((fun x -> x),(fun x -> countD s x d))
    dic

let misMatchFile path =
    let lines = File.ReadAllLines path
    let pos = withMisMatch lines.[1] lines.[0] (int lines.[2]) |> Array.map (fun a -> a.ToString())
    File.WriteAllLines(@"c:\temp\sol5.txt", pos)

let countDFile path =
    let lines = File.ReadAllLines path
    countD lines.[0] lines.[1] (int lines.[2]) 
    
let name = "rosalind_ba1g.txt"
let ham name =
    let strs = File.ReadAllLines(name).Select(fun s -> s.Trim()).ToArray()
    hammingDist strs.[0] strs.[1]