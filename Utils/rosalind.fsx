// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

// Define your library scripting code here
#r @"..\packages\FSharp.Quotations.Evaluator.1.0.6\lib\net40\FSharp.Quotations.Evaluator.dll"

open FSharp.Quotations.Evaluator
open Microsoft.FSharp.Quotations
open System.Reflection
open System.Linq
open System.IO
open System

[<ReflectedDefinition(false)>]
let sq x = x * x

type Sol<'a> = Expr<'a>

//type SolutionBuilder(inp : string, outp : string) =
type SolutionBuilder(inp : string, outp : string, ?isArray) =
    let inp = inp
    let outp = outp
    let isArray = defaultArg isArray false

    member val lines = if isArray then (File.ReadAllLines inp).Select(fun l -> l.Trim()).ToArray() else [||]
    member val arg = if isArray then String.Empty else (File.ReadAllText inp).Trim()

    member this.Bind (sol : Sol<'a>, solve : 'a -> Sol<'b>) =
        let solution = QuotationEvaluator.Evaluate sol
        solve solution

    member this.Return (x : 'a) : Sol<'a> = <@ x @>
    member this.ReturnFrom (s : Sol<'a>) = s
    member this.Run (s : Sol<'a>) = 
        let sol = QuotationEvaluator.Evaluate s

let solution = SolutionBuilder()

solution {
    let! sol = <@ 5 * 2 @>
    let! sol1 = <@ 3 * 2 @>
    return sol + sol1
}


