open System
open System.Linq
open System.Collections.Generic

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