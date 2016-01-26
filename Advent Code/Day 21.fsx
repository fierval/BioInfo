open System
open System.IO

let name = @"c:\users\boris\downloads\input.txt"
let strs = File.ReadAllLines name

let boss = 100, 8, -2

let deal damage armor =
    max (damage + armor) 1

let getMoves pHit bHit pDeal bDeal =
    let pMoves = bHit / pDeal
    let bMoves = pHit / bDeal

    (if pMoves * pDeal = bHit then pMoves else pMoves + 1), if bMoves * bDeal = pHit then bMoves else bMoves + 1
    
let play (player: int * int * int) (boss: int * int * int) =
    let pHit, pDamage, pArmor = player
    let bHit, bDamage, bArmor = boss

    let pDeal = deal pDamage bArmor
    let bDeal = deal bDamage pArmor

    if pHit = bHit then
        pDeal >= bDeal
    else        
        let pMoves, bMoves = getMoves pHit bHit pDeal bDeal
        pMoves <= bMoves

let parse (strs : string []) =
    let parseKind (kindStr : string[]) =
            kindStr 
            |> Array.map
                (fun s -> 
                    let [|_; money; damage; armor|] = s.Trim().Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
                    int money, if int damage = 0 then -int armor else int damage
                 )
    let empties = -1::([0..strs.Length - 1] |> List.filter(fun i -> String.IsNullOrEmpty(strs.[i])))
    [for i = 1 to empties.Length - 1 do
        yield strs.[empties.[i-1] + 1..empties.[i] - 1] |> parseKind
     yield parseKind strs.[empties.[empties.Length - 1] + 1..]
    ]

let playerHits = 100

(*Part 1*)
let solve boss =
    let store = parse strs

    let ringPicks = 
        [for i = 0 to store.[2].Length - 1 do
            for j = 0 to i do
                if i <> j then yield (i, j)
        ]

    [
        for h = 0 to store.[0].Length - 1 do
            for i = 0 to store.[1].Length - 1 do
                for j, k in ringPicks do
                    let pick = [store.[0].[h]; store.[1].[i]; store.[2].[j]; store.[2].[k]]
                    let moneys, action = pick |> List.unzip
                    let money = moneys |> List.sum
                    let armors, damages = action |> List.partition (fun e -> e < 0)
                    let armor, damage = armors |> List.sum, damages |> List.sum
                    if play (playerHits, damage, armor) boss then yield money
    ] |> List.min

(*Part 2*)
let solve2 boss =
    let store = parse strs
    // cartesian product - (i, i)
    let ringPicks = 
        [for i = 0 to store.[2].Length - 1 do
            for j = 0 to i do
                if i <> j then yield (i, j)
        ]

    [
        for h = 0 to store.[0].Length - 1 do
            for i = 0 to store.[1].Length - 1 do
                for j, k in ringPicks do
                    let pick = [store.[0].[h]; store.[1].[i]; store.[2].[j]; store.[2].[k]]
                    let moneys, action = pick |> List.unzip
                    let money = moneys |> List.sum
                    let armors, damages = action |> List.partition (fun e -> e < 0)
                    let armor, damage = armors |> List.sum, damages |> List.sum
                    if not(play (playerHits, damage, armor) boss) then yield money
    ] |> List.max