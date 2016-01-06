module lookAndSayScan
open Alea.CUDA
open Alea.CUDA.Unbound
open Alea.CUDA.Utilities

open System.Diagnostics
open Microsoft.FSharp.Quotations
open System.Collections.Generic
open System.Linq
open System.Windows.Forms
open FSharp.Charting

let worker = Worker.Default
    
[<Kernel; ReflectedDefinition>]
let lookAndSayKernelSimple (arr : deviceptr<int>) (len : int) (out : deviceptr<int>) (flags : deviceptr<int>)=
    let ind = blockIdx.x * blockDim.x + threadIdx.x

    if ind < len then
        let c = arr.[ind]
        let idxOut = 2 * ind
        let prevUnrepeated = if ind = 0 || ind > 0 && arr.[ind - 1] <> c then 1 else 0
        flags.[2 * ind] <- prevUnrepeated
        flags.[2 * ind + 1] <- prevUnrepeated

        if prevUnrepeated = 1 then
            let mutable i = 1
            while ind + i < len && c = arr.[ind + i] do
                i <- i + 1
            out.[idxOut] <- i
            out.[idxOut + 1] <- c
        else
            out.[idxOut] <- 0
            out.[idxOut + 1] <- 0

[<Kernel; ReflectedDefinition>]
let copyScanned (arr : deviceptr<int>) (out : deviceptr<int>) (len : int) (flags : deviceptr<int>) (addrmap : deviceptr<int>) =
    let ind = blockIdx.x * blockDim.x + threadIdx.x

    if ind < len && flags.[ind] > 0 then out.[addrmap.[ind] - 1] <- arr.[ind]
        
let lookAndSayGpuScan (s : string) n =
    let maxLen = 20 * 1024 * 1024
    let arr = s |> Seq.map (fun c -> (string>>int) c) |> Seq.toArray

    use dOut = worker.Malloc(maxLen)
    use dFlags = worker.Malloc(maxLen)
    use dAddressMap = worker.Malloc(maxLen)
    use dArr = worker.Malloc(maxLen)
    dArr.Scatter(arr)

    use scanModule = new DeviceScanModule<int>(GPUModuleTarget.Worker(worker), <@ (+) @>)
    scanModule.Create(100) |> ignore

    let sw = Stopwatch()
    let rec loop n len =
        if n = 0 then len
        else
            let blockSize = 512
            let gridSize = divup len blockSize
            let lp = LaunchParam(gridSize, blockSize)
            use scan = scanModule.Create(2 * len)

            worker.Launch <@lookAndSayKernelSimple@> lp dArr.Ptr len dOut.Ptr dFlags.Ptr
            scan.InclusiveScan(dFlags.Ptr, dAddressMap.Ptr, 2 * len)

            let gridSize = divup (2 * len) blockSize
            let lp = LaunchParam(gridSize, blockSize)

            worker.Launch <@copyScanned@> lp dOut.Ptr dArr.Ptr (2 * len) dFlags.Ptr dAddressMap.Ptr
            let len = dAddressMap.GatherScalar(2 * len - 1)

            loop (n - 1) len

    sw.Start()
    let res = loop n s.Length
    sw.Stop()
    res, sw.ElapsedMilliseconds

let genNext (s : List<byte>) =
    let mutable i = 0y
    let mutable c = s.[0]
    let reps = List<byte>()
    for ch in s do
        if c <> ch then
            reps.Add(byte i)
            reps.Add(c)
            c <- ch
            i <- 0y
        i <- i + 1y
    reps.Add(byte i)
    reps.Add(c)
    reps

let genseq (s : string) n =
    let res =
        {1..n}
        |> Seq.fold 
            (fun st _ -> genNext st) (s |> Seq.map (fun s -> (string>>byte) s) |> fun a -> a.ToList())
        |> fun s -> s.Count
    res

let timeIt e s (f : string -> int -> int) = 
    let sw = Stopwatch()
    {1..3} 
        |> Seq.map (fun _ -> 
                    sw.Reset()
                    sw.Start()
                    f s e |> ignore
                    float sw.ElapsedMilliseconds
                    )
        |> Seq.average

[<EntryPoint>]
let main (argv : string []) =
    let s = "1113122113"
    let range = [40..52]
    for i in 30..32 do
        lookAndSayGpuScan s i |> ignore

    let gpuFast = [for i in range -> i, float (snd (lookAndSayGpuScan s i))] 
    let cpuFast = [for i in range -> i, timeIt i s genseq]

    let myChart = 
        Chart.Combine(
            [Chart.Line(cpuFast, Name="CPU")
             Chart.Line(gpuFast, Name="GPU")   
            ]
        )
            .WithYAxis(Log=false, Title = "msec")
            .WithXAxis(Title = "times")
            .WithLegend(InsideArea=false) 

    let form = new Form(Visible = true, TopMost = true, Width = 700, Height = 500)
    form.Controls.Add(new ChartTypes.ChartControl(myChart, Dock = DockStyle.Fill))
    Application.Run(form);
    printfn "%A" (fst (lookAndSayGpuScan s 50))
    0
