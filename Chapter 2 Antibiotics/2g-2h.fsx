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
    let sol1 = s1.Split(' ') |> Array.map int |> Array.sort 
    let sol2 = s2.Split(' ') |> Array.map int |> Array.sort 
    sol1 = sol2

// get convolution alphabet
// m - elements "with ties"
let convAlphabet (spectrum : int[]) m n=
    let groups = convGroups spectrum |> Seq.filter (fun (k, d) -> k >= weights.[0] && k <= 200)
    let alphabet = groups |> Seq.take m |> Seq.map (fun (k, d) -> k) |> Seq.toArray |> Array.sort
    cyclopeptydeAlphabet alphabet spectrum n

let m = 20
let n = 373
let spectrum = parseSpectrum "853 113 585 796 924 502 423 1210 342 186 761 391 593 1412 1152 1396 260 129 1381 229 242 356 990 1047 57 748 1176 730 990 1038 1119 294 339 114 696 1251 1267 617 567 357 471 163 1266 1281 0 536 1395 454 1104 1362 1039 892 1509 1086 129 649 1095 713 258 777 1394 753 299 599 648 876 414 1249 813 242 859 1305 552 1284 861 650 1249 261 520 470 519 957 1233 405 260 861 762 810 1248 891 916 1346 390 981 147 1323 390 732 618 1380 1038 756 989 225 633 910 204 1452 243 1119 860 1395 129 57 503 1267 1153 276 462 228 1215 114 1170 357 973 388 519 699 131 128 1120 648 1452 1055 632 333 1380 528 747 389 656 97 1167 779 1380 1280 942 115 1121 1152 1007 990 1006 1118 519 877 1378 471"

let compareSpecs (s1 : string) (s2 : string) =
    let sol1 = s1.Split('-') |> Array.map int |> Array.sort 
    let sol2 = s2.Split('-') |> Array.map int |> Array.sort 
    sol1 = sol2

let solveSpec name = 
    let lines = File.ReadAllLines(name)
    let spectrum = parseSpectrum (lines.Last())
    let m = int lines.[0]
    let n = int lines.[1]
    let sol = convAlphabet spectrum m n
    File.WriteAllText(@"c:\temp\ant_2h.txt", sol)
