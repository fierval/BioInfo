open System.IO
#load @"..\Chapter 3 Molecular Clocks\environment.fsx"

let patPos (s : string) (pat : string) = 
    0 |> 
    Seq.unfold 
        (fun state -> 
            let i = s.IndexOf(pat, state)
            if i < 0 then None else Some(i, i + 1)
        )
    |> Seq.toArray

let patFreq (s : string) (pat : string) = 
    (patPos s pat).Length

let solve name = 
    let lines = File.ReadAllLines name
    let sol = patPos lines.[1] lines.[0] |> Seq.fold (fun state e-> state + " " + e.ToString()) System.String.Empty
    File.WriteAllText(@"c:\temp\1c.txt", sol)