open System
open System.Linq
open System.Collections.Generic
open System.IO

#load "2f-CyclospectrumLeaderboard.fsx"
open ``2e-Cyclospectrum``
open ``2f-CyclospectrumLeaderboard``

let convGroups (spectrum : int[]) = 
    spectrum
    |> Array.mapi(fun i s -> spectrum.[i+1..] |> Array.map (fun c -> c - s))
    |> Array.collect (fun d -> d) |> Array.filter (fun d -> d > 0)
    |> Seq.groupBy (fun d -> d) |> Seq.sortBy (fun (k, d) -> -d.Count())

let convolutions (spectrum : int []) = convGroups spectrum |> Seq.collect (fun (k, d) -> d)

let solve name =
    let lines = File.ReadAllText(name)
    let spectrum = parseSpectrum lines
    let sol = 
        (convolutions spectrum)
        |> Seq.fold (fun state e -> state + (if String.IsNullOrEmpty(state) then "" else " ") + e.ToString()) String.Empty
    File.WriteAllText(@"c:\temp\ant_2g.txt", sol.Trim())

let compareSols (s1 : string) (s2 : string) =
    let sol1 = s1.Split(' ') |> Array.map int |> Array.sort |> Array.toList
    let sol2 = s2.Split(' ') |> Array.map int |> Array.sort |> Array.toList
    sol1 = sol2

// get convolution alphabet
// m - elements "with ties"
let convAlphabet (spectrum : int[]) m n=
    let groups = convGroups spectrum |> Seq.filter (fun (k, d) -> k >= weights.[0] && k < weights.Last())
    let alphabet = groups |> Seq.take m |> Seq.map (fun (k, d) -> k) |> Seq.toArray |> Array.sort
    cyclopeptydeAlphabet alphabet spectrum n

let m = 20
let n = 60
let spectrum = parseSpectrum "57 57 71 99 129 137 170 186 194 208 228 265 285 299 307 323 356 364 394 422 493"