open System.IO
open System.Linq
open System

let name = @"c:\users\boris\downloads\input.txt"
let strs = File.ReadAllLines name

let parse (strs : string []) =
    let mol = strs.[strs.Length - 1].Trim()
    let rest = strs.[0..strs.Length - 3]

    let replacements =
        rest
        |> Array.map
            (fun e -> 
                let words = e.Trim().Split([|"=>"; " "|], StringSplitOptions.RemoveEmptyEntries)
                words.First(), words.Last()
            )

    mol, replacements.ToLookup(fst, snd), replacements.ToDictionary(snd, fst)

let replace (mol : string) ind (key : string) y = 
    if ind = 0 then y + mol.[key.Length..]
    elif ind = mol.Length - key.Length then mol.[0..mol.Length - key.Length - 1] + y
    else mol.[0..ind-1] + y + mol.[ind + key.Length..]
    
(* Part 1 *)
let findNumDistinct (strs : string []) =
    let mol, reps, _ = parse strs
    let keys = reps |> Seq.map (fun e -> e.Key)
    [
        for key in keys do
            let mutable stop = false
            let mutable start = 0
            while not stop do
                let ind = mol.IndexOf(key, start)
                if ind > 0 then
                    start <- ind + 1
                    for y in reps.[key] -> replace mol ind key y
                else
                    stop <- true
    ] 
    |> List.distinct
    |> fun l -> l.Length

(* Part 2 *)
let findNumSteps (strs : string []) =
    let mol, reps, revs = parse strs
    let keys = revs |> Seq.map (fun e -> e.Key) |> Seq.sortByDescending (fun k -> k.Length) |> Seq.toArray
    let rec loop st acc =
        if st = "e" then acc
        else
            let mutable ind = -1
            let mutable i = 0
            while ind < 0 do
                ind <- st.IndexOf(keys.[i])
                i <- i + 1

            loop (replace st ind keys.[i-1] revs.[keys.[i - 1]]) (acc + 1)
    loop mol 0