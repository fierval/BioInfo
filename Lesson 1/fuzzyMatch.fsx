open System
open System.Linq
open System.Collections.Generic
open System.IO

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
    pos |> Seq.where(fun p -> p > 0) |> Seq.toArray

let misMatchFile path =
    let lines = File.ReadAllLines path
    let pos = withMisMatch lines.[1] lines.[0] (int lines.[2]) |> Array.map (fun a -> a.ToString())
    File.WriteAllLines(@"c:\temp\sol5.txt", pos)
