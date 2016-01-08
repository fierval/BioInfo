open System
open System.Linq
open System.IO
open System.Collections.Generic
let name = @"c:\users\boris\downloads\input.txt"

let strs = File.ReadAllLines name

type Ability = (int*int*int) []

let parse (strs: string []) : Ability =
    strs
    |> Array.map
        (fun s ->
            let words = s.Trim().Split(' ')
            (int words.[3]), (int words.[6]), (int words.[13])
            )

let distance speed duration rest total =
    let totalJumps = total / (duration + rest)
    let remains = total % (duration + rest)
    totalJumps * speed * duration + (if remains > duration then duration * speed else remains * speed)

let running (abils : Ability) total =
    abils
    |> Array.map (fun (speed, duration, rest) -> distance speed duration rest total)
    |> Array.max

let total = 2503
running (parse strs) total

let runningPoints (abils : Ability) total =
    let rec points (acc : int []) rem =
        if rem = total + 1 then Array.max acc
        else
            let dists = abils |> Array.map (fun (speed, duration, rest) -> distance speed duration rest rem)
            let mDist = Array.max dists
            let pts = dists |> Array.map (fun d -> if d = mDist then 1 else 0)
            let acc = pts |> Array.map2 (fun a d -> a + d) acc
            points acc (rem + 1)

    points (Array.zeroCreate abils.Length) 1

runningPoints (parse strs) total
