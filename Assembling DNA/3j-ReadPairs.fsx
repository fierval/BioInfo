open System
open System.Collections.Generic
open System.Linq

#load "3g-EulerPath.fsx"
#load "3d-3e-debruin.fsx"
#load "3h-3i-Genome.fsx"

open ``3g-EulerPath``
open ``3d-3e-debruin``
open ``3h-3i-Genome``
open System.IO

let genome (nucleotides : string seq) =
    nucleotides |> debruijn |> findPath |> toString

let parseAndSplitPairs (pairs : string seq) =
    let pref = List<string>()
    let suff = List<string>()
    pairs |> Seq.iter (fun s -> 
                        let arr = s.Trim().Split([|'|'|])
                        pref.Add(arr.[0]); suff.Add(arr.[1]))

    pref, suff

let reconstructPath (arr : string seq) d =
    let pref, suff = parseAndSplitPairs arr

    let prefPath = genome pref
    let suffPath = genome suff
    let k = pref.[0].Length

    let prefixCommon = prefPath.Substring(k + d + 1)
    let suffixCommon = suffPath.Substring(0, suffPath.Length - k - d - 1)

    if prefixCommon = suffixCommon then prefPath.Substring(0, k + d) + suffPath else ""

let arr = ["GAGA|TTGA";"TCGT|GATG";"CGTG|ATGT";"TGGT|TGAG";"GTGA|TGTT";"GTGG|GTGA";"TGAG|GTTG";"GGTC|GAGA";"GTCG|AGAT"]
let d = 2

let name = "rosalind_ba3j.txt"

let solve name =
    let str = File.ReadAllLines name
    let [|k; d|] = str.[0].Trim().Split([|' '|], 2) |> Array.map (fun e -> int e)
    let sol = reconstructPath (str.[1..]) d
    File.WriteAllText(@"c:\temp\fromParis.txt", sol)