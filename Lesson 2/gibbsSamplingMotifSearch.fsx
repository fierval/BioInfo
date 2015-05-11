open System
open System.Linq
open System.IO
open System.Diagnostics
open System.Collections.Generic

#load "randomizedMotifSearch.fsx"

open RandomizedMotifSearch
open RegulatoryMotifs
open MostProbableKMer

/// <summary>
/// compute probabilities of all k-mers in the string, given a profile
/// </summary>
/// <param name="s"></param>
/// <param name="profile"></param>
let kmerProbs (s : string) (profile : float [,]) =
    let k = Array2D.length2 profile
    let kmers = [0..s.Length - k].Select(fun i -> s.Substring(i, k)).Distinct().ToArray()
    let probs = 
        kmers 
        |> Array.map 
            (fun kmer -> 
                kmer, kmer.ToCharArray() 
                    |> Array.map (fun c -> alphabet.[c])
                    |> Array.mapi (fun i ind -> profile.[ind, i])
                    |> Array.fold (fun state p -> state * p) 1.)
        
    probs

let gibbsSample (rnd : Random) (probs : (string * float) []) =
    
    let sumall = probs |> Seq.map(fun (k, v) -> v) |> Seq.sum
    let prs = probs |> Array.map (fun (k, v) -> k, v / sumall)
    let len = probs.Length

    let rec rollUntilAccepted value =
        let n = rnd.NextDouble()
        if n <= snd prs.[value] then value
        else
            rollUntilAccepted (rnd.Next(0, len))

    fst probs.[rollUntilAccepted (rnd.Next(0, len))]


let gibbsSamplingMotifSearch (dna : string []) k iters =
    let t = dna.Length

    let rnd = Random(int DateTime.Now.Ticks)
    let len = dna.[0].Length - k

    let firstMotifs = [|1..t|] |> Array.map (fun i -> rnd.Next(0, len)) |> Array.mapi (fun i p -> dna.[i].Substring(p, k) |> toInts)
    let bestMotifs = Array2D.init t k (fun i j -> firstMotifs.[i].[j])
    let bestScore = score bestMotifs

    let rec runSampler (motifs : int [,]) (bestMotifs : int [,]) bestScore n =
        if n = 0 then motifs
        else
            let except = rnd.Next(0, t)
            let profileMotifs = array2D [|motifs.[0..except, 0..] |> Seq.cast; motifs.[except+1.., 0..] |> Seq.cast |]
            let profile = createProfile profileMotifs
            let probs = kmerProbs dna.[except] profile
            let motif = gibbsSample rnd probs |> toInts
            let curMotifs = array2D [|motifs.[0..except, 0..] |> Seq.cast; [|motif|] |> init2D |> Seq.cast; motifs.[except+1.., 0..] |> Seq.cast|]
            let curScore = score motifs

            runSampler curMotifs (if curScore < bestScore then curMotifs else bestMotifs) (if curScore < bestScore then curScore else bestScore) (n - 1)
    runSampler bestMotifs bestMotifs bestScore iters                