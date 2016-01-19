open System
open System.IO
open System.Linq
open System.Collections.Generic

let name = @"c:\users\boris\downloads\input.txt"

let strs = File.ReadAllLines(name)
let data = strs |> Array.map (fun s -> int (s.Trim())) |> Array.toList

let n = 150

(* Part 1 *) 
let rec countDecomp (data : int list) n val'=
    if n = 0 || data.Length = 0 then val'
    else
        [
            for i = 0 to data.Length - 1 do
                yield countDecomp data.[i+1..] (n - data.[i]) (if n = data.[i] then val' + 1 else val')
        ] |> List.sum
                    

(* Part 2 *) 
let rec listDecomp (data : int list) n val' =
    [
        if n = 0 then yield val'
        elif n < 0 then yield []
        else
            for i = 0 to data.Length - 1 do
                yield! listDecomp data.[i+1..] (n - data.[i]) (data.[i] :: val')
    ] |> List.filter (fun l -> l.Length > 0)

let solve (data : int list) n =
    let decomps = listDecomp data n []
    let minDecomp = decomps |> List.minBy (fun l -> l.Length) |> List.length
    decomps
    |> List.filter (fun l -> l.Length = minDecomp)
    |> List.length