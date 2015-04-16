#load "patternFreq.fsx"

open PatternFreq
open System
open System.Collections.Generic
open System.Linq

/// <summary>
/// k-mers within one string and their counts
/// </summary>
/// <param name="s"></param>
/// <param name="k"></param>
let kMers (s : string) k =
    let kmers = [0..s.Length - k].Select(fun i -> s.Substring(i, k))
    let dic = HashSet(kmers).ToDictionary((fun x -> x),(fun x -> patFreq s x))
    dic

/// Most often encountered k-mer sequences in a string
let freqKMer (s : string) k =
    let dic = kMers s k
    let max' = dic.Max(fun kvp -> kvp.Value)
    dic.Where(fun kvp -> kvp.Value = max').Select(fun kvp -> kvp.Key)
