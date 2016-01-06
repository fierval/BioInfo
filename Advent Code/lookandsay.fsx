(*http://adventofcode.com/day/10 *)
#r @"..\packages\Alea.CUDA.2.1.2.3274\lib\net40\Alea.CUDA.dll" 
#r "System.Configuration.dll"
#load @"Day 10.fsx"

open ``Day 10``
open System.IO

open Alea.CUDA
open Alea.CUDA.Utilities
open System.Diagnostics
open FSharp.Charting

Alea.CUDA.Settings.Instance.Resource.AssemblyPath <- Path.Combine(__SOURCE_DIRECTORY__, @"..\packages\Alea.Cuda.2.1.2.3274\private")
Alea.CUDA.Settings.Instance.Resource.Path <- Path.Combine(__SOURCE_DIRECTORY__, @"..\release")

let worker = Worker.Default

[<Kernel; ReflectedDefinition>]
let lookAndSayKernel (arr : deviceptr<int8>) (len : int) (out : deviceptr<int8>) =
    let ind = blockIdx.x * blockDim.x + threadIdx.x

    if ind < len then
        let c = arr.[ind]
        let idxOut = 2 * ind

        if ind = 0 || arr.[ind - 1] <> c then
            let mutable i = 1
            while ind + i < len && c = arr.[ind + i] do
                i <- i + 1
            out.[idxOut] <- int8 i
            out.[idxOut + 1] <- c
        else
            out.[idxOut] <- 0y
            out.[idxOut + 1] <- 0y


let lookAndSayGpu (s : string) n =
    let arr = s |> Seq.map (fun c -> (string>>int8) c) |> Seq.toArray

    let rec loop (arr : int8 []) n =
        if n = 0 then arr.Length
        else
            let blockSize = 512
            let gridSize = divup arr.Length blockSize
            let lp = LaunchParam(gridSize, blockSize)

            use dArr = worker.Malloc(arr)
            use dOut = worker.Malloc(arr.Length * 2)

            worker.Launch <@lookAndSayKernel@> lp dArr.Ptr arr.Length dOut.Ptr

            let out = dOut.Gather()

            let arr = 
                out
                |> Array.filter (fun b -> b > int8 0) 

            loop arr (n - 1)

    loop arr n

// compile the kernel
lookAndSayGpu s 10

let compareGpu (s : string ) (basis : int list) =
    let sw = Stopwatch()
    let timeIt e (f : string -> int -> int) = 
        {1..3} 
            |> Seq.map (fun _ -> 
                        sw.Reset()
                        sw.Start()
                        f s e |> ignore
                        float sw.ElapsedMilliseconds
                        )
            |> Seq.average
    
    let cpp = basis |> Seq.fold (fun st e -> [e, timeIt e genseq] @ st) List.empty
    let gpu = basis |> Seq.fold (fun st e -> [e, timeIt e lookAndSayGpu] @ st) List.empty

    Chart.Combine(
        [Chart.Line(cpp, Name="CPU")
         Chart.Line(gpu, Name="GPU")
        ] 
    )
        .WithYAxis(Log=false, Title = "msec")
        .WithXAxis(Title = "times")
        .WithLegend(InsideArea=false)

compareGpu s [30..51]