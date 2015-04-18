open System.Linq
open System.Collections.Generic
open System

#load "fuzzyMatch.fsx"
#load "reverseCompl.fsx"
open ReverseCompl
open FuzzyMatch

/// <summary>
/// Generate a sequence  of (binary) numbers of size n
/// where #1-bits == l <-- that's "el"
/// </summary>
/// <param name="n">length of the 0, 1 seq</param>
/// <param name="l">max number of 1's</param>
let mutateIdx n l =
    if n > 64 then failwith "Only supporting lengths <=64"
    elif l > n then failwith "l should be <= n"

    let mask = (1UL <<< n) - 1UL
       
    let rec nextNums n l =

        if n = 0 then 
            seq {yield 0UL}
        elif l = 1 then
            0 |> Seq.unfold(fun state -> if state = n then None else Some(1UL <<< state, state + 1))
        else
            seq {
                for i in [0..n - l] do
                    let sq = nextNums (n - i - 1) (l - 1)
                    let sqShift = sq |> Seq.map (fun e -> ((e <<< i + 1) ||| (0x1UL <<< i)) &&& mask)
                    yield! sqShift
            }
    nextNums n l

let alphabet = HashSet([|'A';'C';'G';'T'|])

/// <summary>
/// generate k-mer mutations based on the mutation sequence.
/// Each number in the sequence is a bitmap that describes which indexes in the string should be mutated
/// </summary>
/// <param name="pat"></param>
/// <param name="idx">array, where each element points at a position that needs to be mutated</param>
let mutatePat (pat : string) (idxs : int [][]) = 
    
    let changeChar (s : string) i (c : char) =
        let arr = s.ToCharArray()
        arr.[i] <- c
        String(arr)

    // actually mutate a string given a list of indices to mutate
    let rec replChars (arr : string) (pos : int []) =
        if pos.Length = 1 then
            let p = pos.[0]
            let compl = alphabet.Except([arr.[p]]) 
            compl |> Seq.map(fun c -> changeChar arr p c) 
        else //TODO: this is not ideal: produces many repetitions
            seq {
                for j = 0 to pos.Length - 1 do
                    let preps = alphabet |> Seq.map (fun c -> changeChar arr pos.[j] c)
                    let slicedPos = if j = 0 then pos.[j+1..] elif j = pos.Length - 1 then pos.[..j-1] else pos.[0..j-1].Concat(pos.[j+1..]).ToArray()
                    let final = preps.SelectMany(fun prep -> replChars prep slicedPos)
                    yield! final.Distinct()
            }

    seq {        
        for idx in idxs do
            yield! (replChars pat idx).Distinct()
    }                          


/// <summary>
/// Given a dictionary of k-mers and the d
/// add all possible k-mer mutations where #mutated <= d
/// </summary>
/// <param name="origKMers"></param>
let getMutations (origKmers : Dictionary<string, int>) d =
    // given a shorthand representation of mutation indices
    // explode them into a list
    let mutationIndex n idx =
        [|
            for i in [0..n-1] do
                if ((idx >>> i) &&& 1UL) = 1UL then yield i
        |]

    let n = (origKmers.First().Key.Length)
    let idxes = mutateIdx n d
    let mutIndices = idxes |> Seq.map (fun i -> mutationIndex n i) |> Seq.toArray

    // find all the mutations
    origKmers.SelectMany(fun kvp -> mutatePat kvp.Key mutIndices)
            

/// <summary>
/// find most frequent k-mers with up to d mutated nucleotides
/// </summary>
/// <param name="s"></param>
/// <param name="k"></param>
/// <param name="d"></param>
let findMostFreqMutationsUtil (withReverse : bool) (s : string) k d =
    let initial = kMersMutat s k d
    let mutations = HashSet(getMutations initial d).Except(initial.Keys)

    // not run the mutations
    let mostFreq = mutations.ToDictionary((fun x -> x),(fun x -> countD s x d + if withReverse then countD s (revCompl x) d else 0))
    let all = initial.Concat(mostFreq).ToDictionary((fun kvp -> kvp.Key), (fun (kvp : KeyValuePair<string, int>) -> kvp.Value))
    let max' = all.Max(fun kvp -> kvp.Value)
    all.Where(fun kvp -> kvp.Value = max').Select(fun kvp -> kvp.Key).ToArray()

let findMostFreqMutations = findMostFreqMutationsUtil false
let findMostFreqMutationsRev = findMostFreqMutationsUtil true
