open System.IO
let name = @"c:\users\boris\downloads\input.txt"

let parse name =
    let graph = 
        File.ReadAllLines name 
        |> Array.map 
            (fun s -> 
                let arr = s.Split('=') |> Array.map (fun s -> s.Trim())
                let st, val' = arr.[0], int arr.[1]
                let [|from; _; to'|] = st.Split(' ') |> Array.map (fun s -> s.Trim())
                from, to', val'
                )

    graph |> Array.map (fun (a, b, v) -> [|a; b|]) |> Array.concat |> Array.distinct,
    graph |> Array.fold (fun st (a, b, v) -> Map.add (a, b) v st) Map.empty

(*http://stackoverflow.com/questions/1526046/f-permutations*)  
let walk (nodes : string []) (edges : Map<string * string, int>) =
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
    
    perms (nodes |> Array.toList)

let nodes, edges = parse name 
let all = walk nodes edges

let solve (all : string list list) =
    all
    |> List.map 
        (fun l -> 
            l |> List.mapi 
                (fun i e -> 
                    if i = l.Length - 1 then 0 
                    elif edges.ContainsKey (e, l.[i + 1])
                    then edges.[e, l.[i + 1]]
                    else edges.[l.[i + 1], e]) |> List.sum)
    |> List.max
