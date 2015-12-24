open System
open System.Linq
open System.Collections.Generic
open System.Diagnostics
open System.Text

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
        ([|0..arr.Length - 3|] 
        |> Array.map(fun i -> arr.[i + 2] = arr.[i + 1] - 1uy && arr.[i + 1] = arr.[i] - 1uy)
        |> Array.filter id).Length > 0

    let double =
        ([|0..arr.Length - 2|]
        |> Array.fold 
            (fun (st : Dictionary<byte * byte, int>) i -> 
                if arr.[i] = arr.[i + 1] && not (st.ContainsKey(arr.[i], arr.[i+1])) then st.Add((arr.[i], arr.[i+1]), i)
                st
            ) (Dictionary<byte * byte, int>())).Count > 1
    
    triple && double
        
let solve s =
    let arr = toByte s |> next
    while not (constrained arr) do
        next arr |> ignore

    toStr arr

let s = "cqjxjnds"

