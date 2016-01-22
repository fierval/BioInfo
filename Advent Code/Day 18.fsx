#r @"..\packages\Alea.CUDA.2.1.2.3274\lib\net40\Alea.CUDA.dll" 
#r "System.Configuration.dll"
#load @"..\packages\FSharp.Charting.0.90.12\FSharp.Charting.fsx"
open FSharp.Charting

open System.IO

open Alea.CUDA
open Alea.CUDA.Utilities
open System.Diagnostics
open Microsoft.FSharp.Quotations

let name = @"c:\users\boris\downloads\input.txt"
let strs = File.ReadAllLines name

let parse (strs : string []) =
    let jagged = 
        strs 
        |> Array.map 
            (fun e -> 
                e.Trim().ToCharArray()
                |> Array.map 
                    (fun c ->
                        if c = '.' then 0uy else 1uy)
            )
    Array2D.init strs.Length jagged.[0].Length (fun i j -> jagged.[i].[j])

Alea.CUDA.Settings.Instance.Resource.AssemblyPath <- Path.Combine(__SOURCE_DIRECTORY__, @"..\packages\Alea.Cuda.2.1.2.3274\private")
Alea.CUDA.Settings.Instance.Resource.Path <- Path.Combine(__SOURCE_DIRECTORY__, @"..\release")

let worker = Worker.Default

let toArr2d (arr : byte []) rows cols=
    Array2D.init rows cols (fun i j -> arr.[i * cols + j])

[<ReflectedDefinition>]
let sumNeighbors rows cols row col (arr : deviceptr<uint8>) =
    let mutable sum = 0uy
    for i = -1 to 1 do
        let x = row + i
        for j = - 1 to 1 do
            let y = col + j
            if i <> 0 || j <> 0 then
                if x < rows && y < cols && x >= 0 && y >= 0 then
                    let idx = cols * x + y
                    sum <- sum + arr.[idx]
    sum

[<ReflectedDefinition>]
let outVal curVal neighborsOn =
    if curVal = 1uy then
        if neighborsOn = 2uy || neighborsOn = 3uy then curVal else 0uy
    else
        if neighborsOn = 3uy then 1uy else 0uy

(* Part 1 *)
[<Kernel;ReflectedDefinition>]
let animateKernel (orig : deviceptr<uint8>) (out : deviceptr<uint8>) rows cols =
    let x = blockDim.x * blockIdx.x + threadIdx.x

    if x < rows then
        let y = blockDim.y * blockIdx.y + threadIdx.y

        if y < cols then
            let neighborsOn = sumNeighbors rows cols x y orig
            let idx = x * cols + y
            out.[idx] <- outVal orig.[idx] neighborsOn

(* Part 2 *)
[<ReflectedDefinition>]
let isOn row col rows cols =
    (row = col && (row = 0 || row = rows - 1)) || (row = 0 && col = cols - 1) || (col = 0 && row = rows - 1)

[<Kernel;ReflectedDefinition>]
let animateKernel2 (orig : deviceptr<uint8>) (out : deviceptr<uint8>) rows cols =
    let x = blockDim.x * blockIdx.x + threadIdx.x

    if x < rows then
        let y = blockDim.y * blockIdx.y + threadIdx.y

        if y < cols then
            let idx = x * cols + y
            if not (isOn x y rows cols) then
                let neighborsOn = sumNeighbors rows cols x y orig
                out.[idx] <- outVal orig.[idx] neighborsOn
            else
                out.[idx] <- 1uy

let animate (strs : string []) n (kernel : Expr<(deviceptr<uint8> -> deviceptr<uint8> -> int -> int-> unit)>) part2 =
    let arr = parse strs
    let cols = Array2D.length2 arr
    let rows = Array2D.length1 arr
    if part2 then
        arr.[rows - 1, 0] <- 1uy
        arr.[0, cols - 1] <- 1uy
        arr.[0, 0] <- 1uy
        arr.[rows - 1, cols - 1] <- 1uy

    let blockSize = dim3(32, 32)
    let gridSize = dim3(divup rows blockSize.x, divup cols blockSize.y)
    let lp = LaunchParam(gridSize, blockSize)

    use dArr = new DeviceArray2D<byte>(worker, rows, cols)
    dArr.Scatter(arr)
    use dOutput = new DeviceArray2D<byte>(worker, rows, cols)

    for i = 1 to n do
        let dIn, dOut = if i &&& 0x1 = 1 then dArr, dOutput else dOutput, dArr
        worker.Launch kernel lp dIn.Ptr dOut.Ptr rows cols

    let out = if n &&& 0x1 = 1 then dOutput.Gather() else dArr.Gather()
    out |> Seq.cast<byte> |> Seq.toArray |> Array.map int |> Array.sum

let animateCPU (strs : string []) n =
    let mutable arr = parse strs
    let cols = Array2D.length2 arr
    let rows = Array2D.length1 arr

    let sumNeighbors rows cols row col (a : byte[,]) =
        let mutable sum = 0uy
        for i = -1 to 1 do
            let x = row + i
            for j = - 1 to 1 do
                let y = col + j
                if i <> 0 || j <> 0 then
                    if x < rows && y < cols && x >= 0 && y >= 0 then
                        sum <- sum + a.[x,y]
        sum
    
    for k = 1 to n do
        arr <- arr|> Array2D.mapi (fun i j e -> outVal e (sumNeighbors rows cols i j arr))

    arr |> Seq.cast<byte> |> Seq.map int |> Seq.sum

let sw = Stopwatch()
animate strs 100 <@animateKernel@> false |> ignore
sw.Restart()
animate strs 100 <@animateKernel@> false |> ignore
sw.Stop()
printfn "%A" sw.Elapsed

animateCPU strs 100 |> ignore
sw.Restart()
animateCPU strs 100 |> ignore
sw.Stop()
printfn "%A" sw.Elapsed



