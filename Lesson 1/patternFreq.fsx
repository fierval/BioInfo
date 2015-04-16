open System
open System.Linq
open System.Collections.Generic

let patFreq (s : string) (pat : string) = 
    let mutable count = 0
    let mutable stop = false
    let mutable i = 0
    while not stop do
        let j = (s.IndexOf(pat, i))
        if j > 0 && i < s.Length - pat.Length then
            i <- j + 1
            count <- count + 1
        else
            stop <- true

    count

let patPos (s : string) (pat : string) = 
    let mutable count = 0
    let mutable stop = false
    let pos = List<int>()
    while not stop do
        let j = if pos.Count > 0 then s.IndexOf(pat, pos.Last() + 1) else s.IndexOf(pat)
        if j > 0 then
            pos.Add(j)
        else
            stop <- true

    pos.ToArray()
