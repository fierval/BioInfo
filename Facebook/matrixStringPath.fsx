open System
open System.Collections.Generic
open System.Linq

let findNextPos i j (positions : List<int*int>) (visited : HashSet<int*int>) =
    let res = List<int>()
    for k = 0 to positions.Count - 1 do
        let x, y = positions.[k]
        if (x = i - 1 && y = j) || (x = i + 1 && y = j) || (x = i && y = j + 1) || (x = i && y = j - 1) && not (visited.Contains((x, y))) then res.Add(k)
    res

let walkPath (matr : char [,]) (str : string) =
    let str = str.ToUpper()
    let path = List<int*int>()
    let startMap = Dictionary<char, List<int * int>>()
    let visited = HashSet<int*int>()
    
    matr 
    |> Array2D.iteri 
        (fun i j s -> 
            if not (startMap.ContainsKey s) then startMap.Add(s, List<int*int>()) 
            startMap.[s].Add(i, j))

    let toVisit = Stack<List<int*int>>()
    let chars = str.ToCharArray()
    if chars.Count(fun c -> not (startMap.ContainsKey c)) > 0 then failwith "no path"

    let mutable k = 0
    let mutable var = 0

    while k < chars.Length do
        let positions = startMap.[chars.[k]]
        if k = 0 then
            if positions.Count = 0 then failwith "no path"
            toVisit.Push(positions.Select(id).ToList())
            k <- k + 1
        else            
            //get next position from the stack
            let variations = toVisit.Peek()
            
            let i, j = variations.[var]

            visited.Add(i, j) |> ignore
            let nextPos = findNextPos i j positions visited

            if nextPos.Count > 0 then
                path.Add(i, j)
                toVisit.Push(nextPos.Select(fun n -> positions.[n]).ToList())
                k <- k + 1
                if k = chars.Length then
                    path.Add(positions.[nextPos.[0]])
                else
                    var <- 0
            else
                if toVisit.Count = 0 then failwith "path does not exist"
                else
                    var <- var + 1
                    if var = variations.Count - 1
                    then
                        for i, j in variations do
                            visited.Remove(i, j) |> ignore
                        toVisit.Pop() |> ignore
                        path.RemoveAt(path.Count - 1)
                    k <- k - 1
    path.ToArray()

let parseStrs (strs : string seq) =
    let strs = strs |> Seq.map (fun e -> e.ToUpper()) |> Seq.toList
    Array2D.init (strs.Count()) (strs.First().Length) (fun i j -> strs.[i].[j])

let strs = ["ABCE"; "SFCS"; "ADEE"]

let str = "bcced"

let matr = parseStrs strs