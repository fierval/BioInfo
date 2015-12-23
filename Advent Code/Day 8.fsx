open System.IO
open System.Collections.Generic
open System.Linq
open System
open System.Text.RegularExpressions

let name = @"c:\users\boris\downloads\input.txt"
let pat = "(\\\\x[0-f][0-f])|(\\\\\\\\)|(\\\\\")"

let solve name pat = 
    let lines = File.ReadAllLines(name).Select(fun s -> s.Trim().Substring(1, s.Length - 2)).ToArray()
    let mcs = lines |> Array.map (fun s -> Regex.Matches(s, pat))

    mcs
    |> Array.filter (fun mc -> mc.Count > 0)
    |> Array.map (fun mc -> [for m in mc do yield (m.Length - 1)] |> Seq.sum)
    |> Array.sum
    |> (+) (2 * lines.Length)
    

let solve2 name pat = 
    let lines = File.ReadAllLines(name).Select(fun s -> s.Trim().Substring(1, s.Length - 2)).ToArray()
    let mcs = lines |> Array.map (fun s -> Regex.Matches(s, pat))
    let numCode = 4 * lines.Length

    mcs
    |> Array.filter (fun mc -> mc.Count > 0)
    |> Array.map 
        (fun mc -> 
            [for m in mc do 
                if m.Length = 2 then yield 2
                else
                    yield 1] |> Seq.sum)
    |> Array.sum
    |> (+) numCode
    
