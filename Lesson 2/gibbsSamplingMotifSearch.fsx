open System
open System.Linq
open System.IO

#load "randomizedMotifSearch.fsx"
#load "..\packages\MathNet.Numerics.FSharp.3.6.0\MathNet.Numerics.fsx"

open MathNet.Numerics.Random

open RandomizedMotifSearch
open RegulatoryMotifs
open MostProbableKMer

let rndSeed = RandomSeed.Robust()
let rnd = Random.mersenneTwisterSeed rndSeed

/// <summary>
/// compute probabilities of all k-mers in the string, given a profile
/// </summary>
/// <param name="s"></param>
/// <param name="profile"></param>
let kmerProbs (kmers : string []) (profile : float [,]) =
    let k = Array2D.length2 profile
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


let gibbsSamplingMotifSearch (dna : string []) k iters rndStarts =
    let t = dna.Length
    let kmers = dna |> Array.map(fun s -> [0..s.Length - k].Select(fun i -> s.Substring(i, k)).Distinct().ToArray())

    let len = dna.[0].Length - k

    let runSingle () = 
        let firstMotifs = [|1..t|] |> Array.map (fun i -> rnd.Next(0, len)) |> Array.mapi (fun i p -> dna.[i].Substring(p, k) |> toInts)
        let bestMotifs = array2D firstMotifs
        let bestScore = score bestMotifs

        let rec runSampler (motifs : int [,]) (bestMotifs : int [,]) bestScore n =
            if n = 0 then bestScore, bestMotifs
            else
                let except = rnd.Next(0, t)
                let profileMotifs = stackV motifs.[0..except-1, *] motifs.[except+1.., *] 
                let profile = createProfile profileMotifs
                let probs = kmerProbs kmers.[except] profile
                let motif = array2D [|gibbsSample rnd probs |> toInts|]
                let curMotifs = motifs.[except+1.., *] |> stackV motif  |> stackV motifs.[0..except-1, *]  
                let curScore = score motifs

                runSampler curMotifs (if curScore < bestScore then curMotifs else bestMotifs) (if curScore < bestScore then curScore else bestScore) (n - 1)
        runSampler bestMotifs bestMotifs bestScore iters

    let scoresMotifs = [1..rndStarts].AsParallel().Select(fun _ -> runSingle()).ToArray()
    
    // if we have more than one sequence of the same score - select the most frequent one    
    let sc, motifs = scoresMotifs |> Array.minBy (fun (sc, m) -> sc)
        
    sc, [|0..t-1|] |> Array.map (fun m -> toStr motifs.[m, *])


let gibbsSamplingFile file =
    let lines = File.ReadAllLines(file)
    let ktIters = lines.[0].Split(' ') |> Array.map int
    let k, _, iters = ktIters.[0], ktIters.[1], ktIters.[2]
    let rndStarts = 20
    let dna = lines.[1..]
    sw.Reset()
    sw.Start()
    let sc, motifs = gibbsSamplingMotifSearch dna k iters rndStarts
    sw.Stop()
    printfn "Elapsed: %s" (sw.Elapsed.ToString())
    File.WriteAllLines(@"c:\temp\sol7.txt", motifs)