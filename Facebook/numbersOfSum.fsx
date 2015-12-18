open System.Collections.Generic
(*http://codercareer.blogspot.com/2011/10/no-09-numbers-with-given-sum.html*)

(*
Given an increasingly sorted array and a number s, please find two numbers whose sum is s. If there are multiple pairs with sum s, just output any one of them.

For example, if the inputs are an array {1, 2, 4, 7, 11, 15} and a number 15, please out two numbers 4 and 11 since 4+11=15.
*)
let numOfSum2 (arr : int []) s =
    let arr = arr |> Array.sort
    let sums = HashSet<int>()

    let mutable stop = false
    let mutable i = 0
    while not stop && i < arr.Length do
        if sums.Contains arr.[i] then stop <- true
        else
            sums.Add (s - arr.[i]) |> ignore
        i <- i + 1
        
    if not stop then None else Some (s - arr.[i - 1], arr.[i - 1])

let numOfSum3 (arr : int []) s =
    let arr = arr |> Array.sort

    let rec numOfSum3rec i = 
        let arr1 = if i = 0 then arr.[1..] elif i = arr.Length - 1 then arr.[0..arr.Length-2] else Array.concat [|arr.[0..i-1]; arr.[i+1..arr.Length - 1]|]

        let rest = numOfSum2 arr1 (s - arr.[i]) 
        
        if rest <> None then 
            Some (
                match rest with
                | Some (a, b) -> a, b, arr.[i]
                | _ -> failwith "not possible"
            )
        elif i = arr.Length - 1 then None
        else
            numOfSum3rec (i + 1) 

    numOfSum3rec 0