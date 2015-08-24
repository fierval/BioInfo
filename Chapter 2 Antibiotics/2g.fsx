open System
open System.Linq
open System.IO

#load "2e-Cyclospectrum.fsx"
open ``2e-Cyclospectrum``

let convolutions (spectrum : int []) =
    let diffs = 
        spectrum
        |> Array.mapi
            (fun i s -> spectrum.[i+1..] |> Array.map (fun c -> c - s))
        |> Array.collect (fun d -> d) |> Array.filter (fun d -> d > 0)
    
    let convs = diffs |> Seq.groupBy (fun d -> d) |> Seq.sortBy (fun (k, d) -> -d.Count()) |> Seq.collect (fun (k, d) -> d)
    convs

let spectrum = parseSpectrum "465 473 998 257 0 385 664 707 147 929 87 450 748 938 998 768 234 722 851 113 700 957 265 284 250 137 317 801 128 820 321 612 956 434 534 621 651 129 421 337 216 699 347 101 464 601 87 563 738 635 386 972 620 851 948 200 156 571 551 522 828 984 514 378 363 484 855 869 835 234 1085 764 230 885"

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