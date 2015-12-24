(*http://adventofcode.com/day/2 *)
open System.IO
open System

let name = @"c:\users\boris\downloads\input.txt"

let parse name = 
    let strs = File.ReadAllLines name
    let measurements = strs |> Array.map (fun s -> s.Trim().Split('x') |> Array.map int)
    measurements


let totSqFeet (measurements : int [][]) =
    measurements 
    |> Seq.map 
        (fun e -> 
            let one, two, three = e.[0] * e.[1], e.[1] * e.[2], e.[0] * e.[2]
            let minOne = min three (min one two)
            minOne + 2 *one + 2 * two + 2 * three
        ) 
    |> Seq.sum

let totRibon (measurements : int [][]) =
    measurements
    |> Seq.map
        (fun e -> 
            let one, two, three = e.[0] + e.[1], e.[1] + e.[2], e.[0] + e.[2]
            let minOne = min three (min one two)
            2 * minOne + e.[0] * e.[1] * e.[2]
        ) 
    |> Seq.sum
