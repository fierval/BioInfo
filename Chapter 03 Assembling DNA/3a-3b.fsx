open System
open System.IO
open System.Linq

Environment.CurrentDirectory <- @"c:\users\boris\downloads"

let decomposeAndSort shouldSort (s : string) k =
    let decomp = 
        s |> Seq.unfold (fun st -> if st.Length < k then None else Some(st.Substring(0, k), st.Substring(1))) |> Seq.toArray
    if shouldSort then decomp |> Array.sort else decomp

let decomposeRosalind = decomposeAndSort true
let decompose = decomposeAndSort false

let recompose (pats : string []) =
    pats |> Seq.fold(fun st e -> if String.IsNullOrEmpty st then e else st + e.Last().ToString()) String.Empty

let pats = [|"ACCGA"; "CCGAA"; "CGAAG"; "GAAGC"; "AAGCT"|]

let solve name =
    let lines = File.ReadAllLines(name)
    let k = int (lines.[0].Trim())
    let sol = decomposeRosalind (lines.[1].Trim()) k

    File.WriteAllLines(@"c:\temp\decomp.txt", sol)

let solveRec name =
    let lines = File.ReadAllLines(name)
    let pats = lines |> Array.map (fun e -> e.Trim([|' '; '\t'; '\n'; '\r'|]))

    let sol = recompose pats

    File.WriteAllText(@"c:\temp\recomp.txt", sol)