open System

let generateString len =
    let alphabet = ['A'; 'C'; 'G'; 'T']

    let rnd = Random(int DateTime.Now.Ticks)
    String(len |> Seq.unfold (fun state -> if state = 0 then None else Some(alphabet.[rnd.Next(alphabet.Length)], state - 1)) |> Seq.toArray)
