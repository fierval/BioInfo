#load "patternFreq.fsx"

open System
open System.IO
open PatternFreq

let alphabet = ['A'; 'C'; 'G'; 'T']
let numAlphabet = dict[('A', 0L); ('C', 1L); ('G', 2L); ('T', 3L)]

let generateKmers k =
    let rec generate (kmers : string seq) k =
        if k = 0 then kmers
        else
            let expanded = 
                kmers 
                |> Seq.map 
                    (fun kmer -> alphabet |> Seq.map (fun a -> kmer + a.ToString()))
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

let patToNum (pattern : string) =
    pattern.ToCharArray()
    |> Array.fold (fun state c -> (state <<< 2) + numAlphabet.[c]) 0L

let numToPat (n : int64) k =
    let sol = 
        n 
        |> Seq.unfold 
            (fun state -> 
                if state = 0L then None
                else
                    Some(alphabet.[int (state &&& 3L)], state >>> 2))
        |> Seq.toList
        |> List.rev
    
    let sol = if sol.Length < k then alphabet.[0]::sol else sol
    String(sol |> Seq.toArray)