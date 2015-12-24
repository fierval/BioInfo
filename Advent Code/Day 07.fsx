(*http://adventofcode.com/day/7 *)
open System.IO
open System.Collections.Generic
open System.Linq
open System

let name = @"c:\users\boris\downloads\input.txt"

let channels = Dictionary<string, uint16>()

[<FlagsAttribute>]
type Op =
| OR = 0
| AND = 1
| LSHIFT = 2
| RSHIFT = 3

type Expr =
| LVAR of string
| NOT of string
| OP of Op * string * string

let parse (instr : string) =
    let [|left; right|] = instr.Split([|"->"|], StringSplitOptions.None) |> Array.map (fun s -> s.Trim())

    let ast = Stack<Expr>()        

    let tokens = left.Split(' ') |> Array.map (fun s -> s.Trim())
    let lval = 
        if tokens.[0] = "NOT" then
            NOT(tokens.[1])
        elif tokens.Length = 1 then
            LVAR(tokens.[0])
        else
            OP(Enum.Parse(typeof<Op>, tokens.[1]) :?> Op, tokens.[0], tokens.[2])

    right, lval
            

let rec exec right (instrs : Dictionary<string, Expr>) =
    printfn "Solving for: %s with %A" right instrs.[right]
    let left = instrs.[right]
    
    let getValue s =
        let res, v = UInt16.TryParse(s)
        if res then res, v
        elif channels.ContainsKey s then
            true, channels.[s]
        else false, 0us
    

    match left with
    | LVAR name -> 
        let res, v = getValue name
        if res then channels.Add(right, v); v
        else exec name instrs
    | NOT expr -> 
        let res, v = getValue expr
        if res then channels.Add(right, ~~~v); ~~~v
        else ~~~(exec expr instrs)
    | OP (op, e1, e2) ->
        let res1, v1 = getValue e1
        let res2, v2 = getValue e2
        match op with
            | Op.OR -> 
                if res1 && res2 then channels.Add(right, v1 ||| v2); v1 ||| v2
                else
                    (if res1 then v1 else (exec e1 instrs)) ||| (if res2 then v2 else (exec e2 instrs))
            | Op.AND  -> 
                if res1 && res2 then channels.Add(right, v1 &&& v2); v1 &&& v2
                else                
                    (if res1 then v1 else (exec e1 instrs)) &&& (if res2 then v2 else (exec e2 instrs))
            | Op.RSHIFT -> 
                if res1 && res2 then channels.Add(right, v1 >>> int v2); v1 >>> int v2
                else
                    (if res1 then v1 else (exec e1 instrs)) >>> (if res2 then int v2 else int (exec e2 instrs))
            | Op.LSHIFT -> 
                if res1 && res2 then channels.Add(right, v1 <<< int v2); v1 <<< int v2
                else
                    (if res1 then v1 else (exec e1 instrs)) <<< (if res2 then int v2 else int (exec e2 instrs))
        
let solve key name =
    exec key ((File.ReadAllLines name |> Array.map parse).ToDictionary(fst, snd))

let solve2 keyOverride key name =
    channels.Clear()
    let overrideValue = solve key name    
    channels.Clear()
    let instrs = ((File.ReadAllLines name |> Array.map parse).ToDictionary(fst, snd))
    instrs.Remove(keyOverride) |> ignore
    channels.Add(keyOverride, overrideValue)
    exec key instrs

let key = "a"
let keyOverride = "b"