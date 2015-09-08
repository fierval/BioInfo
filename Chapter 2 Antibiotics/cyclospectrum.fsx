#r @"..\packages\Alea.CUDA.2.1.2.3274\lib\net40\Alea.CUDA.dll" 
#r "System.Configuration.dll"
#r @"..\packages\NUnit.2.6.4\lib\nunit.framework.dll"
#r @"..\packages\FsUnit.1.3.1.0\lib\FsUnit.NUnit.dll"
#load @"..\packages\FSharp.Charting.0.90.12\FSharp.Charting.fsx"

#load "DataStructs.fsx"

open System.Linq
open System
open System.Collections.Generic
open System.IO
open DataStructs
open FsUnit

open Alea.CUDA
open Alea.CUDA.Utilities
open System.Diagnostics
open FSharp.Charting

Alea.CUDA.Settings.Instance.Resource.AssemblyPath <- Path.Combine(__SOURCE_DIRECTORY__, @"..\packages\Alea.Cuda.2.1.2.3274\private")
Alea.CUDA.Settings.Instance.Resource.Path <- Path.Combine(__SOURCE_DIRECTORY__, @"..\release")

// array of amino acid weights
let weights = (aminoAcidOneLetterIntegerMassTrunc |> Seq.map (fun kvp -> kvp.Value)) |> Seq.toArray

// brute force O(n^3) solution
let cyclospec (peptide : int seq) =
    let len = peptide.Count()
    let pepArr = peptide |> Seq.toArray

    let mutable parsum = 0

    (seq {
        yield 0
        yield pepArr |> Array.sum
        for i = 0 to len - 2 do
            yield! [0..len-1] 
                |> Seq.map 
                    (fun j ->  
                        //F# 4.0 Feature!
                        parsum <- 0
                        for ind = j to j + i do
                            parsum <- parsum + pepArr.[if ind >= len then ind - len else ind]
                        parsum    
                        )
    }) |> Seq.sort |> Seq.toArray

// O(n^2) solution
let cyclospecOpt (peptide : int seq) =
    let len = peptide.Count()
    let pepArr = peptide |> Seq.toArray

    let output = Array.zeroCreate ((len - 1) * len)

    for i = 0 to len - 2 do
        [0..len-1] 
            |> Seq.iter 
                (fun j ->  
                    let ind = i + j
                    output.[i * len + j] <- 
                        if i = 0 then pepArr.[j] 
                        else output.[(i - 1) * len + j] + pepArr.[if ind >= len then ind - len else ind]
                    )
    seq {yield 0; yield pepArr |> Array.sum; yield! output} |> Seq.sort |> Seq.toArray

// generates a peptide for comparison
let generatePeptide (len : int) =
    let rnd = Random(int DateTime.UtcNow.Ticks)

    seq {
        for i in [1..len] -> weights.[rnd.Next(weights.Length)]
    } |> Seq.sort |> Seq.toArray

// GPU worker
let worker = Worker.Default

[<Kernel; ReflectedDefinition>]
let cyclospecKernel (arr : deviceptr<int>) (len : int) (out : deviceptr<int>) =
    let ind = blockIdx.x * blockDim.x + threadIdx.x
    let lenElem = blockIdx.y * blockDim.y + threadIdx.y

    if ind < len && lenElem < len - 1 then
        let mutable parsum = 0
        for i = 0 to lenElem do
            let idx = ind + i
            parsum <- parsum + arr.[if idx >= len then idx - len else idx]
        out.[lenElem * len + ind] <- parsum

let cyclospecGpu (peptide : int []) =
    let blockSize = dim3(16, 16, 1)
    let gridSize = dim3(divup peptide.Length blockSize.x, divup peptide.Length blockSize.y)
    let lp = LaunchParam(gridSize, blockSize)

    use dPeptide = worker.Malloc(peptide)
    use dOutput : DeviceMemory<int> = worker.Malloc(peptide.Length * (peptide.Length - 1))
    worker.Launch <@cyclospecKernel @> lp dPeptide.Ptr peptide.Length dOutput.Ptr
    let output = dOutput.Gather()

    seq{yield 0; yield! output; yield peptide |> Seq.sum} |> Seq.toArray |> Array.sort


let test len =
    let peptide = generatePeptide len
    let cpu = cyclospec peptide
    let cpuOpt = cyclospecOpt peptide
    let gpu = cyclospecGpu peptide

    should equal cpu gpu
    should equal cpu cpuOpt 

// experiment: run from 10 ** low to 10 ** high array length
let experiment low high =
    if low >= high || low < 0 then failwith "must be: low < high, both non-negative"

    // salt it
    let len = 100
    test len

    let sw = Stopwatch()
    let cpuTimes = Array.zeroCreate (high - low + 1)
    let cpuOptTimes = Array.zeroCreate (high - low + 1)
    let gpuTimes = Array.zeroCreate (high - low + 1)

    for i = low to high do
        let range = 500 * i
        let arr = generatePeptide range
        let idx = i - low

        // Run on CPU
        sw.Restart()
        printfn "Legnth: %d" range
        printfn "-----------"
        let h1 = cyclospec arr
        sw.Stop()

        cpuTimes.[idx] <- range, sw.Elapsed.TotalSeconds
        printfn "Computed on CPU: %0.5f sec" (snd cpuTimes.[idx])

        // Run on CPU (optimized)
        sw.Restart()
        let h3 = cyclospecOpt arr
        sw.Stop()

        cpuOptTimes.[idx] <- range, sw.Elapsed.TotalSeconds
        printfn "Computed on CPU (optimized): %0.5f sec" (snd cpuOptTimes.[idx])

        //Run on GPU
        sw.Restart()
        let h2 = cyclospecGpu arr
        sw.Stop()
        gpuTimes.[idx] <- range, float sw.Elapsed.TotalSeconds
        printfn "Computed on GPU: %0.5f sec" (snd gpuTimes.[idx])
        printfn ""

    Chart.Combine(
        [Chart.Line(cpuTimes, Name="CPU")
         Chart.Line(cpuOptTimes, Name="CPU (optimized)")
         Chart.Line(gpuTimes, Name="GPU")
        ] 
    )
        .WithYAxis(Log=true, Title = "sec")
        .WithXAxis(Min=500. * float low, Title = "length")
        .WithLegend(InsideArea=false) 
