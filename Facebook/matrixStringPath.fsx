open System
open System.Collections.Generic
open System.Linq

let findNextPos i j (positions : List<int*int>) (visited : HashSet<int*int>) =
    let mutable stop = false
    let mutable k = 0
    let res = List<int>()
    while k < positions.Count do
        let x, y = positions.[k]
        if (x = i - 1 && y = j) || (x = i + 1 && y = j) || (x = i && y = j + 1) || (x = i && y = j - 1) && not (visited.Contains((x, y))) then res.Add(k)
        k <- k + 1
    res

let walkPath (matr : char [,]) (str : string) =
    let str = str.ToUpper()
    let path = List<int*int>()
    let startMap = Dictionary<char, List<int * int>>()
    let visited = HashSet<int*int>()
    let depths = Stack<int>()
    
    matr 
    |> Array2D.iteri 
        (fun i j s -> 
            if not (startMap.ContainsKey s) then startMap.Add(s, List<int*int>()) 
            startMap.[s].Add(i, j))

    let toVisit = Stack<int*int>()
    let chars = str.ToCharArray()
    if chars.Count(fun c -> not (startMap.ContainsKey c)) > 0 then failwith "no path"

    let mutable k = 0
    let curVisited = List<int*int>()

    while k < chars.Length do
        let positions = startMap.[chars.[k]]
        if k = 0 then
            positions |> Seq.iter (fun pos -> toVisit.Push pos)
            depths.Push(toVisit.Count)
            k <- k + 1
                    
        //get next position from the stack
        let i, j = toVisit.Pop()
        path.Add(i, j)
        if k < chars.Length - 1 then
            let nextPos = findNextPos i j positions visited
            visited.Add(i, j) |> ignore
            curVisited.Add(i,j)

            if nextPos.Count > 0 then
                nextPos |> Seq.iter (fun pos -> toVisit.Push positions.[pos])
                depths.Push(nextPos.Count)
                k <- k + 1
            else
                if toVisit.Count = 0 then failwith "path does not exist"
                else
                    path.RemoveAt(path.Count - 1)
                    depths.Push(depths.Pop() - 1)
                    if depths.Peek() = 0
                    then
                        depths.Pop() |> ignore
                        for i, j in curVisited do
                            visited.Remove(i, j) |> ignore
                            curVisited.Clear()
                    k <- k - 1

let parseStrs (strs : string seq) =
    let strs = strs |> Seq.map (fun e -> e.ToUpper()) |> Seq.toList
    Array2D.init (strs.First().Length) (strs.Count()) (fun i j -> strs.[i].[j])

let strs = ["ABCE"; "SFCS"; "ADEE"]