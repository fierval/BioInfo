open System
open System.Linq
open System.IO
open System.Collections.Generic
let name = @"c:\users\boris\downloads\input.txt"

type Sitting = Map<string * string, int>

let distrib e L =
    let rec aux pre post = 
        [
            match post with
            | [] -> yield (L @ [e])
            | h::t -> yield (List.rev pre @ [e] @ post)
                      yield! aux (h::pre) t 
        ]
    aux [] L

let rec perms = function 
    | [] -> List.singleton []
    | h::t -> List.collect (distrib h) (perms t)

let parse (strs : string []) : Sitting * string list =
    let raw =
        strs 
        |> Array.map
            (fun s ->
                let words = s.Trim([|' '; '.'|]).Split(' ')
                let key = words.[0], words.Last()
                let val' = if words.[2] = "gain" then int words.[3] else -int words.[3]
                key, val'
            )
    let map = raw |> Map.ofArray
    let names = raw |> Array.toList |> List.map (fun ((a, b), _) -> [a; b]) |> List.concat |> List.distinct
    map, names    


let strs = File.ReadAllLines(name)

let sittings (edges : Sitting) (names : string list) =
    perms names 
    |> List.map 
        (fun permutation -> 
            permutation
                |> List.windowed 2
                |> List.map (fun [a; b] -> [edges.[(a, b)]; edges.[(b,a)]])
                |> List.concat
                |> fun l -> edges.[permutation.[permutation.Length - 1], permutation.[0]] :: edges.[permutation.[0], permutation.[permutation.Length - 1]] :: l
                |> List.sum
        )
    |> List.max
             
let edges, names = parse strs
sittings edges names

(*Part 2*)
let withMe (names : string list) (edges : Sitting) =
    let namesMe =
        names
        |> List.map (fun e -> [((e, "Me"), 0); (("Me", e), 0)]) |> List.concat

    let mapList = edges |> Map.toList
    let fullEdges = [mapList; namesMe] |> List.concat |> Map.ofList
    let fullNames = "Me"::names

    sittings fullEdges fullNames