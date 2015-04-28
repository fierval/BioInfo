open System.Linq
open System.Collections.Generic
open System

#load "fuzzyMatch.fsx"
#load "reverseCompl.fsx"
open ReverseCompl
open FuzzyMatch
open System.Diagnostics

let changeChar (s : string) i (c : char) =
    let arr = s.ToCharArray()
    arr.[i] <- c
    String(arr)

let alphabet = HashSet([|'A';'C';'G';'T'|])

/// <summary>
/// generate k-mer mutations based on the mutation sequence.
/// Each number in the sequence is a bitmap that describes which indexes in the string should be mutated
/// </summary>
/// <param name="pat"></param>
/// <param name="idx">array, where each element points at a position that needs to be mutated</param>
let mutatePat (s : string) d =

    if d <= 0 || d > s.Length then failwith "d must be positive and less than string length"
    if String.IsNullOrWhiteSpace(s) then failwith "string must make sense"

    let rec mutateStr (sub : string) d =
        if d = 1 then
            seq {
                for i = 0 to sub.Length - 1 do
                    yield! alphabet.Except [sub.[i]] |> Seq.map (fun c -> changeChar sub i c)
            }
        else
            seq {
                let compl = alphabet.Except [sub.[0]] |> Seq.toArray
                let mut = compl |> Seq.map (fun c -> mutateStr sub.[1..] (d - 1)) |> Seq.collect(fun sq -> sq)
                yield! alphabet |> Seq.map (fun c -> mut |> Seq.map (fun st -> sub.[0].ToString() + st)) |> Seq.collect (fun s -> s)
            }
    mutateStr s d |> Seq.distinct;;

/// <summary>
/// Given a dictionary of k-mers and the d
/// add all possible k-mer mutations where #mutated <= d
/// </summary>
/// <param name="origKMers"></param>
let getMutations (origKmers : Dictionary<string, int>) d =
    // find all the mutations
    origKmers.SelectMany(fun kvp -> mutatePat kvp.Key d)
            

/// <summary>
/// find most frequent k-mers with up to d mutated nucleotides
/// </summary>
/// <param name="s"></param>
/// <param name="k"></param>
/// <param name="d"></param>
let findMostFreqMutationsUtil (withReverse : bool) (s : string) k d =
    let sw = Stopwatch()
    sw.Start()
    let initial = kMersMutat s k d
    let mutations = HashSet(getMutations initial d).Except(initial.Keys)

    let mostFreq = mutations.ToDictionary((fun x -> x),(fun x -> countD s x d + if withReverse then countD s (revCompl x) d else 0))
    let all = initial.Concat(mostFreq).ToDictionary((fun kvp -> kvp.Key), (fun (kvp : KeyValuePair<string, int>) -> kvp.Value))
    let max' = all.Max(fun kvp -> kvp.Value)
    let res = all.Where(fun kvp -> kvp.Value = max').Select(fun kvp -> kvp.Key).ToArray()
    printfn "Elapsed: %s" (sw.Elapsed.ToString())
    res

let findMostFreqMutations = findMostFreqMutationsUtil false
let findMostFreqMutationsRev = findMostFreqMutationsUtil true

