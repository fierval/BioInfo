#load @"..\Chapter 1 DnaA Box\mostFreqKMerWithD.fsx"

open System.IO
open System.Linq
open System.Collections.Generic
open MostFreqKMerWithD
open FuzzyMatch

/// <summary>
/// Brute force finding of k-mers with d-mutations
/// </summary>
/// <param name="dna"></param>
/// <param name="k"></param>
/// <param name="d"></param>
let findKMersBrute (dna : string []) k d =
    // per string, get all the mutations that appear at least once in each string
    let init = 
        (seq {
            for s in dna do
                let initial = kMersMutat s k d
                let mutations = (getMutations initial d).Distinct().Except(initial.Keys)

                // not run the mutations
                let countsMut = 
                    mutations
                        .ToDictionary((fun x -> x),(fun x -> countD s x d))
                        .Where(fun kvp -> kvp.Value > 0)
                        .Select(fun kvp -> kvp.Key)

                let all = countsMut.Union(initial.Keys).ToArray()
                yield! all
       }).Distinct()
    
    // get only those that appear in all strings
    let kmers =
        init.AsParallel().Where(fun km -> dna.AsParallel().Select(fun s -> countD s km d).Count(fun c -> c > 0) = dna.Length)
    kmers.ToArray()

let findKmersBruteFile fileName =
    let inp = File.ReadAllLines(fileName)
    let kd = inp.[0].Trim().Split([|' '|], 2)
    let k, d = int (kd.First()), int (kd.Last())
    let res = findKMersBrute inp.[1..] k d
    File.WriteAllLines(@"c:\temp\sol21.txt", res)


let dna1 = [|
            "ATTTGGC"
            "TGCCTTA"
            "CGGTATC"
            "GAAAATT"
            |]

let k, d = 3, 1

// test on an extra dataset
let fileName = @"c:\temp\extra.txt"
findKmersBruteFile fileName

let expected = @"c:\temp\expected.txt"
let actual = @"c:\temp\sol21.txt"

let comp (exp : string []) (act : string []) =
    if exp.Length <> act.Length then failwith "Different counts"
    let expSet = HashSet(exp)
    if act.Except(exp).Union(exp.Except(act)).Count() > 0 then failwith "Different sets"


let exp = File.ReadAllText(expected).Split([|'\r'; '\n'; ' '|])
let act = File.ReadAllLines(actual)