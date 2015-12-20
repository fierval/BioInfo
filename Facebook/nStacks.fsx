open System.Linq
open System.Collections.Generic

let mutable N = 0
let mutable minAlloc = 10
let descriptors = Dictionary<int, int * int>()
let mutable totalFree = 0
let mutable lastDict = -1

let orderedDescs () =
    descriptors |> Seq.sortBy (fun kvp -> snd kvp.Value) |> Seq.map (fun kvp -> kvp.Key) |> Seq.toList

// compact the memory and ensure we have at least one element
// at the end of i
let compactMemory (stack : 'a []) j =
    let ordered = orderedDescs()

    //compactMemory should not be called for total current stacks < 2
    let tail = min minAlloc (totalFree / (descriptors.Count - 1))
    
    [|
        let mutable curStart = 0
        for i = 0 to ordered.Length - 1 do
            let start, end' = descriptors.[i]
            descriptors.[i] <- (curStart, curStart + end' - start)
            curStart <- snd descriptors.[i] + tail - 1
            yield Array.concat[|stack.[start..end']; Array.create (if tail = 0 && i = j then 1 else tail) Unchecked.defaultof<'a>|]
    |] |> Array.collect id 

let createNew (stack : 'a []) i =
    if descriptors.Count = 0
    then
        descriptors.Add(i, (0, 0))
        lastDict <- i
        stack
    else
        let len = stack.Length
        let prev = orderedDescs().Last()
        let stack = if (snd descriptors.[prev]) > len - 1 then compactMemory stack i else stack
            
        let start = (snd descriptors.[prev]) + min (minAlloc - 1) (len - (snd descriptors.[prev]))
        descriptors.Add(i, (start, start))
        stack
            
let squeezeIn (stack : 'a []) i =
    let ordered = orderedDescs().ToList()
    let cur = ordered.IndexOf(i)
    let end' = snd descriptors.[i]
    let next = if cur = ordered.Count - 1 then stack.Length - 1 else fst descriptors.[ordered.[cur - 1]]
    
    if end' + 1 = next then compactMemory stack i else stack

let init<'a> size n minMem =
    if size < 0 then failwith "size < 0"
    if n < 2 || n > size / 2 then failwith "size matters"
    if minMem < 1 then failwith "minMem < 1"

    // actual stack
    let stack = Array.create size Unchecked.defaultof<'a>

    // free space and position of the head
    N <- n    
    totalFree <- size
    minAlloc <- minMem
    stack

let push (stack : 'a []) i (o : 'a) =
    if totalFree = 0 then failwith "overflow"
    totalFree <- totalFree - 1

    // stack does not exist
    let stack = 
        if not (descriptors.ContainsKey i) then
            createNew stack i
        else
            squeezeIn stack i
    let start, end' = descriptors.[i]
    let pos = end' + 1
    stack.[pos] <- o
    descriptors.[i] <- (start, pos)
    stack

let pop (stack : 'a []) i =
    if not (descriptors.ContainsKey i) then failwith "stack does not exist"
    else
        totalFree <- totalFree + 1
        let start, end' = descriptors.[i]
        if end' = start then descriptors.Remove(i) |> ignore
        else
            descriptors.[i] <- (start, end' - 1)
        stack.[end'], stack