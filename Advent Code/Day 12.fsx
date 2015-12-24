open System
open System.Text.RegularExpressions
open System.IO
open System.Collections.Generic
open System.Linq

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

let solve2 name =
    let json = File.ReadAllText name
    let mutable raw = getSum json
    // find all reds
    let mcreds = Regex.Matches(json, ":\\\"red")
    let reds = Stack([for m in mcreds -> m.Index] |> List.rev)

    let mutable j = reds.Peek()
    let mutable stop = false

    while not stop do
        if reds.Count = 0 then stop <- true
        while reds.Peek() < j do
            reds.Pop() |> ignore
        j <- reds.Pop()
        let mutable braces = 1
        let jBrace = json.LastIndexOf("{", j)
        j <- j + ":\"red\"".Length 
        while braces > 0 do
            if json.[j] = '}' then braces <- braces - 1
            j <- j + 1
        let tainted = json.Substring(jBrace, j - jBrace + 1)
        raw <- raw - getSum tainted
    raw