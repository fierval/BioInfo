open System
open System.IO

let name = @"c:\users\boris\downloads\input.txt"

let strs = File.ReadAllLines(name)
let n = 100

let parse (strs : string [])=
    strs
    |> Array.map 
        (fun s ->
            let words = s.Trim().Split([|':'; ' '; ','|])
            [|int words.[3]; int words.[6]; int words.[9]; int words.[12]; int words.[15]|]
            )

let rec splitNum j k =
    [
        if k = 1 then yield [j]
        else
            for num = 1 to j - 1 do
                let numl = splitNum (j - num) (k - 1)
                for n in numl do
                    yield num::n
    ]

(* Part 1 *)    
let solve (strs : string []) n =
    let combs = splitNum n strs.Length
    let vals = parse strs
    let prods = 
        combs
        |> List.map
            (fun comb ->
                [for i = 0 to vals.[0].Length - 2 do
                    yield [
                        for j = 0 to comb.Length - 1 do
                            yield comb.[j] * vals.[j].[i]
                    ] |> List.sum
                ] |> List.reduce (fun st e -> st * (if e < 0 then 0 else e)), comb
       )
    prods |> List.maxBy (fun (p, l) -> p)


(* Part 2 *)
let solveCal (strs : string []) n cal =
    let combs = splitNum n strs.Length
    let vals = parse strs
    let prods = 
        combs
        |> List.map
            (fun comb ->
                [for i = 0 to vals.[0].Length - 1 do
                    yield [
                        for j = 0 to comb.Length - 1 do
                            yield comb.[j] * vals.[j].[i]
                    ] |> List.sum 
                ] |> List.rev 
       )
    prods 
    |> List.filter (fun l -> l.[0] = cal) 
    |> List.map (fun l -> List.tail l |> List.reduce (fun st e -> st * (if e < 0 then 0 else e)))
    |> List.max