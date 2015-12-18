open System
open System.Collections.Generic
open System.Linq

let findNextPos i j (positions : List<int*int>) (visited : bool [,]) =
    let mutable stop = false
    let mutable k = 0
    let res = List<int>()
    while k < positions.Count do
        let x, y = positions.[k]
        if (x = i - 1 && y = j) || (x = i + 1 && y = j) || (x = i && y = j + 1) || (x = i && y = j - 1) && not visited.[x, y] then res.Add(k)
        k <- k + 1
    res

let walkPath (matr : char [,]) (str : string) =
    let len1 = Array2D.length1 matr
    let len2 = Array2D.length2 matr
    let path = List<int*int>()
    let startMap = Dictionary<char, List<int * int>>()
    let visited = Array2D.create len1 len2 false
    matr 
    |> Array2D.iteri 
        (fun i j s -> 
            if not (startMap.ContainsKey s) then startMap.Add(s, List<int*int>()) 
            startMap.[s].Add(i, j))

    let toVisit = Stack<int*int>()
    let chars = str.ToCharArray()
    if chars.Count(fun c -> not (startMap.ContainsKey c)) > 0 then failwith "no path"

    let mutable k = 0

    while k < chars.Length do
        let positions = startMap.[chars.[k]]
        if k = 0 then
            let i, j = positions.[0]
            path.Add(i,j)
            positions.RemoveAt(0)
            positions |> Seq.iter (fun pos -> toVisit.Push pos)
            visited.[i,j] <- true
        else
            let curI, curJ = path.Last()
            let nextPos = findNextPos curI curJ positions visited
            if nextPos.Count > 0 then
                let i, j = positions.[nextPos.[0]]
                path.Add(i,j)
                visited.[i, j] <- true
                nextPos.RemoveAt(nextPos.[0])
                nextPos |> Seq.iter (fun pos -> toVisit.Push positions.[pos])
                k <- k + 1
            else
                if toVisit.Count = 0 then failwith "path does not exist"
                else
                    path.RemoveAt(path.Count - 1)
                    toVisit.Pop() |> ignore
                    k <- k - 1

let parseStrs (strs : string seq) =
    let strs = Seq.toList strs
    Array2D.init (strs.First().Length) (strs.Count()) (fun i j -> strs.[i].[j])

let strs = ["ABCE"; "SFCS"; "ADEE"]