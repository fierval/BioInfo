(*http://adventofcode.com/day/3 *)
open System.IO
open System

let name = @"c:\users\boris\downloads\input.txt"
type Direction =
| North
| South
| East
| West

let pDir (str : string) =
    str.ToCharArray() 
    |> Array.map 
        (fun c ->
            if c = '^' then North
            elif c = '>' then East
            elif c = '<' then West
            elif c = 'v' then South
            else failwith "wrong direction"
        )

let parse name =
    File.ReadAllText name |> pDir

let move (point : int * int) dir =
    let x, y = match point with  x, y -> x, y

    match dir with
    | North -> (x, y + 1)
    | South -> (x, y - 1)
    | East -> (x + 1, y)
    | West -> (x - 1, y)


let deliverTo (dirs : Direction []) =
    let rec keepMoving (dirs : Direction []) (acc : (int * int) list) =
        if dirs.Length = 0 then acc
        else
            keepMoving (dirs.[1..]) ((move acc.[0] dirs.[0])::acc)

    (keepMoving dirs [0,0] |> List.distinct).Length

let bothDeliverTo (dirs : Direction []) =
    let rec keepMoving (dirs : Direction []) (acc : (int * int) list) =
        if dirs.Length = 0 then acc
        else
            let point1 = move acc.[0] dirs.[0]
            let point2 = move acc.[1] dirs.[1]
            let acc = point1 :: (point2 :: acc)
            keepMoving (dirs.[2..]) acc

    (keepMoving dirs [0,0; 0,0] |> List.distinct).Length
    