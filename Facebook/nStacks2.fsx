open System.Collections.Generic

let mutable N = 0
let descriptors = Dictionary<int, int>(N)
let mutable positions : int [] = Array.empty
let mutable totalFree = 0
let mutable curFree : bool [] = Array.empty

let init<'a> size n =
    if size < 0 then failwith "size < 0"
    if n < 2 || n > size / 2 then failwith "size matters"

    // actual stack
    let stack = Array.create size Unchecked.defaultof<'a>
    positions <- Array.create size -1
    curFree <- Array.create size true

    // free space and position of the head
    N <- n    
    totalFree <- size
    stack

let findFree () = 
    let mutable i = 0
    while not curFree.[i] && i < curFree.Length do
        i <- i + 1
    i

let push (stack : 'a []) i (v : 'a) =
    if totalFree = 0 then failwith "overflow"
    else
        totalFree <- totalFree - 1
        let indFree = findFree()

        if not (descriptors.ContainsKey i) then
            descriptors.Add(i, indFree)
            positions.[indFree] <- -1
        else
            let pos = descriptors.[i]
            positions.[indFree] <- pos
            descriptors.[i] <- indFree

        stack.[indFree] <- v
        curFree.[indFree] <- false

    stack
            
let pop (stack : 'a []) i =
    if not (descriptors.ContainsKey i) then failwith "unknown stack"
    else
        let top = descriptors.[i]
        totalFree <- totalFree + 1
        curFree.[top] <- true
        if positions.[top] = -1 then descriptors.Remove i |> ignore
        else
            descriptors.[i] <- positions.[top]
        stack.[top], stack

let stack = init<int> 10 4