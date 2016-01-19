#r @"..\packages\Alea.CUDA.2.1.2.3274\lib\net40\Alea.CUDA.dll" 
#r "System.Configuration.dll"
#load @"..\packages\FSharp.Charting.0.90.12\FSharp.Charting.fsx"
open FSharp.Charting

open System.IO

open Alea.CUDA
open Alea.CUDA.Utilities
open System.Diagnostics

Alea.CUDA.Settings.Instance.Resource.AssemblyPath <- Path.Combine(__SOURCE_DIRECTORY__, @"..\packages\Alea.Cuda.2.1.2.3274\private")
Alea.CUDA.Settings.Instance.Resource.Path <- Path.Combine(__SOURCE_DIRECTORY__, @"..\release")

let worker = Worker.Default

[<ReflectedDefinition>]
let sumNeighbors rows cols row col (arr : deviceptr<uint8>) =
    let mutable sum = 0uy
    for i = -1 to 1 do
        let x = row + i
        for j = - 1 to 1 do
            let y = col + j
            if x <> 0 || y <> 0 then
                if x < rows && y < cols && x >= 0 && y >= 0 then
                    let idx = cols * x + y
                    sum <- sum + arr.[idx]
    sum

[<Kernel;ReflectedDefinition>]
let animate (orig : deviceptr<uint8>) (out : deviceptr<uint8>) rows cols =
    let x = blockDim.x * blockIdx.x + threadIdx.x

    if x < rows then
        let y = blockDim.y * blockIdx.y + threadIdx.y

        if y < cols then
            let neighborsOn = sumNeighbors rows cols x y orig
            let idx = x * cols + y
            let curVal = orig.[idx]
            let outVal = 
                if curVal = 1uy then
                    if neighborsOn = 2uy || neighborsOn = 3uy then curVal else 0uy
                else
                    if neighborsOn = 3uy then 1uy else 0uy
            out.[idx] <- outVal