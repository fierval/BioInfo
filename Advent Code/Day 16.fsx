open System
open System.IO
open System.Linq
open System.Collections.Generic

let name = @"c:\users\boris\downloads\input.txt"

let strs = File.ReadAllLines(name)

let children = "children"
let cats = "cats"
let samoyeds = "samoyeds"
let pomeranians = "pomeranians"
let akitas = "akitas"
let vizslas = "vizslas"
let goldfish = "goldfish"
let trees = "trees"
let cars = "cars"
let perfumes = "perfumes"

let criterion = 
    dict [(children, 3); (cats, 7); (samoyeds, 2); (pomeranians, 3); (akitas, 0); (vizslas, 0);
            (goldfish, 5); (trees, 3); (cars, 2); (perfumes, 1)                
    ]

let parse (strs : string []) =
    strs
    |> Array.map
        (fun s ->
            let words = s.Trim().Split(' ', ',', ';', ':') |> Array.filter (fun s -> not (String.IsNullOrEmpty s))
            words.[2..]
            |> Array.windowed 2
            |> Array.mapi (fun i [|key; v|] -> if i &&& 0x1 > 0 then ("", -1) else (key, int v))
            |> Array.filter (fun (x, _) -> not (String.IsNullOrEmpty x))
            |> Map.ofArray
            )

let data = parse strs

let filter key val' =
    data |> Array.filter (fun m -> m.ContainsKey key && m.[key] = val')

(* Part 2 *)
let maybeAunt =
    let exact = criterion |> Seq.filter(fun kvp -> kvp.Key <> cats && kvp.Key <> trees && kvp.Key <> pomeranians && kvp.Key <> goldfish) |> fun s -> s.ToDictionary((fun kvp -> kvp.Key), (fun (kvp : KeyValuePair<string, int>) -> kvp.Value))

    let gt = criterion |> Seq.filter (fun kvp -> kvp.Key = cats || kvp.Key = trees)
            |> fun s -> s.ToDictionary((fun kvp -> kvp.Key), (fun (kvp : KeyValuePair<string, int>) -> kvp.Value))

    let lt = criterion |> Seq.filter (fun kvp -> kvp.Key = pomeranians || kvp.Key = goldfish)
            |> fun s -> s.ToDictionary((fun kvp -> kvp.Key), (fun (kvp : KeyValuePair<string, int>) -> kvp.Value))

    data
    |> Array.mapi (fun i e -> i + 1, e)
    |> Array.filter 
        (fun (i, m) ->
                [
                    for c in criterion.Keys do
                        let failedExact = m.ContainsKey c && exact.ContainsKey c && m.[c] <> exact.[c]
                        let failedGt = m.ContainsKey c && gt.ContainsKey c && m.[c] <= gt.[c]
                        let failedLt = m.ContainsKey c && lt.ContainsKey c && m.[c] >= lt.[c]
                        yield failedExact || failedGt || failedLt
                ] |> List.filter id |> List.length = 0
        )