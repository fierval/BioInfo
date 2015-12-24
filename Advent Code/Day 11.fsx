(*http://adventofcode.com/day/11 *)
open System
open System.Linq
open System.Collections.Generic

let start = byte 'a'
let finish = byte 'z'
let len = finish - start
let excluded = HashSet<byte>([byte 'i'; byte 'o'; byte 'l'])

let next (arr : byte []) =
    let mutable stop = false
    let mutable i = 0
    while not stop && i < arr.Length do
        arr.[i] <- arr.[i] + 1uy
        if excluded.Contains arr.[i] then
            arr.[i] <- arr.[i] + 1uy
            stop <- true
        if arr.[i] > finish then
            arr.[i] <- start
            i <- i + 1
        else
            stop <- true
    arr

let toByte (s : string) =
    s.ToCharArray().Select(fun c -> byte c).Reverse().ToArray()

let toStr (arr : byte []) =
   String(arr |>  Array.map char |> Array.rev)

let constrained (arr : byte []) =
    let triple = 
        arr
        |> Array.windowed 3 
        |> Array.exists(fun [|c; b; a|] -> b = a + 1uy && c = b + 1uy)

    let double =
        arr
        |> Array.windowed 2
        |> Array.filter(fun [|a; b|] -> a = b)
        |> Array.distinct 
        |> Array.length
        |> fun l -> l > 1
    
    triple && double
        
let solve s =
    let arr = toByte s |> next
    while not (constrained arr) do
        next arr |> ignore

    toStr arr

let s = "cqjxjnds"

