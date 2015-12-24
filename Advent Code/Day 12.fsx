open System.Text.RegularExpressions
open System.IO

let name = @"c:\users\boris\downloads\input.txt"

let pat = "(-?[0-9]+)"

let getSum json = 
    let mc = Regex.Matches(json, pat)
    let nums = 
        [for m in mc do
            yield int m.Value
        ] 
    if nums.Length = 0 then 0 else List.sum nums

let solve name =
    File.ReadAllText name |> getSum

let move (json : string) j back =
    let mutable jPos = j
    let inc = if back then (-) else (+)
    let dec = if back then (+) else (-)
    let mutable braces = 1

    while braces > 0 do
        if json.[jPos] = '}' then braces <- dec braces 1
        elif json.[jPos] = '{' then braces <- inc braces 1
        jPos <- inc jPos 1
    if back then jPos + 1 else jPos

let solve2 name =
    let json = File.ReadAllText name
    
    // find all reds
    let bounds =
        Regex.Matches(json, ":\"red\"")
        |> Seq.cast<Match>
        |> Seq.map (fun m -> (move json m.Index true), (move json m.Index false))
        |> Seq.toArray
               
    Regex.Matches(json, pat)
    |> Seq.cast<Match>
    |> Seq.filter 
        (fun m -> 
            not <| Array.exists (fun (start, end') -> m.Index > start && m.Index < end') bounds
        )
    |> Seq.sumBy (fun m -> int m.Value)
