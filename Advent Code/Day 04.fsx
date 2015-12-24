open System.Security.Cryptography
open System.Text
open System
open System.Diagnostics

let md5 = MD5.Create()

let valid5 (hash : byte []) =
    hash.Length = 16 && hash.[0] = hash.[1] && hash.[1] = 0uy && hash.[2] < 16uy

let valid6 (hash : byte []) =
    hash.Length = 16 && hash.[0] = hash.[1] && hash.[1] = 0uy && hash.[2] = 0uy

let fstHash (key : string) =
    let key = Encoding.UTF8.GetBytes key
    let mutable i = 1
    let mutable stop = false
    let sw = Stopwatch()
    sw.Start()
    
    while not stop && i <= Int32.MaxValue do
        let hash = md5.ComputeHash ([|key; Encoding.UTF8.GetBytes(i.ToString())|] |> Array.concat)
        if valid6 hash then stop <- true
        else
            i <- i + 1
    sw.Stop()
    printfn "Time elapsed: %A" sw.Elapsed
    i         