open System
open System.Linq
open System.Collections.Generic
open System.IO

#load "2e-Cyclospectrum.fsx"
open ``2e-Cyclospectrum``

let convolutions (spectrum : int []) =
    let diffs = 
        spectrum
        |> Array.mapi
            (fun i s ->
                spectrum.[i+1..] |> Array.map (fun c -> c - s))
        |> Array.collect (fun d -> d)
    
    let convs = diffs |> Seq.groupBy (fun d -> d) |> Seq.sortBy (fun (k, d) -> -d.Count()) |> Seq.collect (fun (k, d) -> d)
    convs |> Seq.toArray

let spectrum = parseSpectrum "0 137 186 323"