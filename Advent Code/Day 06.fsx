(*http://adventofcode.com/day/6 *)
open System.IO

let name = @"c:\users\boris\downloads\input.txt"

let lights = Array2D.create 1000 1000 false
let newLights : int [,] = Array2D.zeroCreate 1000 1000

type rect = {left: int; right: int; top: int; bottom: int}

type Instruction =
| On
| Off
| Toggle

let parse (lines : string []) =

    lines
    |> Array.map (fun s -> 
                    let parts = s.Trim().Split(' ')
                    let isSmall = parts.Length = 4
                    let i = if isSmall then 1 else 2
                    let instr = if isSmall then Toggle elif parts.[1] = "on" then On else Off
                    let [|left; top|]  = parts.[i].Split(',') |> Array.map int
                    let [|right; bottom|] = parts.[i + 2].Split(',') |> Array.map int
                    let r = {left = left; right = right; top = top; bottom = bottom}
                    instr, r
        )
let setRange instr r =
    for i = r.left to r.right do
        for j = r.top to r.bottom do
            lights.[i, j] <- 
                match instr with
                | On -> true
                | Off -> false
                | Toggle -> not lights.[i,j]

let newSetRange instr r =
    for i = r.left to r.right do
        for j = r.top to r.bottom do
            newLights.[i, j] <- 
                match instr with
                | On -> newLights.[i,j] + 1
                | Off -> max 0 (newLights.[i,j] - 1)
                | Toggle -> newLights.[i,j] + 2
    
let solve name =
    File.ReadAllLines name |> parse
    |> Seq.iter (fun instr -> newSetRange (fst instr) (snd instr))

    seq {for i = 0 to 999 do
            for j = 0 to 999 do
                if newLights.[i,j] > 0 then yield newLights.[i,j]} |> Seq.sum
    