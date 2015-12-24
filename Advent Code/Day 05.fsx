(*http://adventofcode.com/day/5 *)
open System.Collections.Generic
open System.Linq
open System.IO
open System

let name = @"c:\users\boris\downloads\input.txt"

let vowels = HashSet("aeiou".ToCharArray())
let doubles = 
    ["ab"; "cd"; "pq"; "xy"] 
    |> List.map (fun s -> 
                    let a = s.ToCharArray() |> Array.map int
                    a.[0], a.[1]) |> HashSet

let isNice (s : string) =
    let sarr = s.ToCharArray()
    let numArr = sarr |> Array.map int

    let hasVowel = sarr.Count(fun c -> vowels.Contains c) > 2
    let numDouble = numArr.[0..sarr.Length - 2] |> Array.mapi (fun i s -> (s, numArr.[i + 1]))
    let hasDouble = numDouble.Count(fun (i, j) -> i = j) > 0
    let noConseq = numDouble.Count(fun (i, j) -> j = i + 1 && doubles.Contains((i,j))) = 0

    hasDouble && hasVowel && noConseq

let isNice2 (s : string) =
    let sarr = s.ToCharArray()
    
    let hasConseq (l : int []) =
        l.Length = 2 && l.[0] = l.[1] - 1

    let pairs = 
        sarr.[0..sarr.Length - 2]
            .Select(fun s i -> s, sarr.[i + 1], i)
            .GroupBy(fun (a, b, i) -> a,b)
            .Where(fun gr -> gr.Count() > 1)
            .Select(fun gr -> gr.Select(fun (a, b, i) -> i).OrderBy(fun i -> i).ToArray()).ToArray()
    
    let hasDoubles = pairs.Length > 0 && pairs.Count(fun l -> hasConseq l) = 0
    hasDoubles &&
        sarr.[0..sarr.Length - 3].Select(fun e i -> e, sarr.[i + 1], sarr.[i + 2]).Count(fun (a, b, c) -> a = c) > 0

let solve name =
    let strs = File.ReadAllLines name

    strs.Count(Func<string, bool>(isNice2))