#load "patternFreq.fsx"

open System
open System.IO
open PatternFreq

let alphabet = ["A"; "C"; "G"; "T"]

let generateKmers k =
    let rec generate (kmers : string seq) k =
        if k = 0 then kmers
        else
            let expanded = 
                kmers 
                |> Seq.map 
                    (fun kmer -> alphabet |> Seq.map (fun a -> kmer + a))
                |> Seq.collect(fun s -> s)
            generate expanded (k - 1)
    
    generate [String.Empty] k                
    
let freqArray (pattern : string) k =
    let kmers = generateKmers k
    kmers |> Seq.map(fun k -> patFreq pattern k) |> Seq.toArray

let solve name = 
    let lines = File.ReadAllLines name
    let sol = freqArray lines.[0] (int lines.[1]) |> Array.fold (fun state i -> state + " " + (string i)) String.Empty
    File.WriteAllText(@"c:\temp\1h.txt", sol.TrimStart())        