// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.
open System

let pow (x : int) (y : int) = int (float x ** float y)
let pow10 = pow 10

let serializeDigits n =
    let serialize p =
        let boundary = pow10 p
        if p = 0 then [0..9]
        else
            let nums = [boundary..boundary * 10 - 1]
            nums |> List.map (fun e -> e.ToString().ToCharArray() |> Array.map (string >> int) |> Array.toList) |> List.collect id    

    0 |> Seq.unfold (fun k -> if k = n then None else Some (serialize k, k + 1)) |> Seq.collect id |> Seq.toArray

(*http://codercareer.blogspot.com/2013/02/no-38-digits-in-sequence.html
Numbers are serialized increasingly into a sequence in the format of 0123456789101112131415..., which each digit occupies a position in the sequence. For instance, the digit in the position 5 is 5, in the position 13 is 1, in the position 19 is 4, and so on.

Please write a function/method to get the digit at any given position.
 *)
let digitAt index =
    let normalizeIndex index =
        let rec normIndex index p acc =
            let acc = 9 * (pow10 p) * (p + 1)
            if index < acc then index, (p + 1)
            else
                normIndex (index - acc) (p + 1) acc

        if index < 10 then index, 1
        else
            normIndex (index - 10) 1 10

    let digitAtNormalized (index, p) =
        if p <= 1 then index
        else
            let digitIndex = index % p
            let num = (pow10 (p - 1)) + (index / p)
            let digits = num.ToString().ToCharArray() |> Array.map (string >> int)
            digits.[digitIndex]

    index |> normalizeIndex |> digitAtNormalized