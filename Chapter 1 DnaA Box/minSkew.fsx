#r @"C:\Users\Boris\Documents\GitHub\Extension-Method-Pack\NorthHorizon.Common.Xmp\bin\Debug\NorthHorizon.Common.Xmp.dll"
#load @"..\Chapter 3 Molecular Clocks\environment.fsx"

open System
open System.Linq
open System.IO

/// <summary>
/// return the skew(i) for a s string.
/// </summary>
/// <param name="s"></param>
/// <param name="i"></param>
let skew (s: string) =
    let pickOneMinusOneZero i =
        if i = 0 then 0
        else
            let c = s.[i - 1]
            if c = 'C' then -1 elif c = 'G' then 1 else 0

    let buildSkew = 
        (0, 0) 
        |> Seq.unfold(fun (skew, i) -> if i = s.Length + 1 then None else Some((skew + pickOneMinusOneZero i), ((skew + pickOneMinusOneZero i), i + 1)))
        |> Seq.toArray

    buildSkew

let minSkew (sk : #seq<int>) =
    let min' = sk.Min()
    let skList = sk.ToList()
    seq {for i = 0 to skList.Count - 1 do
            if skList.[i] = min' then yield i
        }    

let solve f =
    let lines = File.ReadAllText(f).Trim() |> skew |> minSkew |> Seq.toArray |> Array.map (fun i -> i.ToString())
    File.WriteAllLines(@"c:\temp\sol4.txt", lines) 