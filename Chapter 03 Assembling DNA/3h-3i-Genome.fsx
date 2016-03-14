open System
open System.Linq
open System.IO

#load "3g-EulerPath.fsx"
#load "3d-3e-debruin.fsx"

open ``3g-EulerPath``
open ``3f-EulerCycle``
open ``3d-3e-debruin``

let name = "rosalind_ba3h.txt"

let toString (cycle : string seq) =
    cycle.Skip(1) |> Seq.fold (fun acc e -> acc + e.Last().ToString()) (cycle.First())

let solve name =
    let sol = (File.ReadAllLines name).[1..] |> debruijn |> findPath |> toString
    File.WriteAllText(@"c:\temp\genome.txt", sol)

let n = 9
let universalCircular (n : int) =
    let maxN = int (2. ** (float n))
    let kmers = maxN |> Seq.unfold (fun cur -> if cur = 0 then None else Some(Convert.ToString(cur - 1, 2).PadLeft(n, '0'), cur - 1))

    let sol = kmers |> debruijn |> findCycle |> toString
    File.WriteAllText(@"c:\temp\universal.txt", sol.Substring(0, maxN)) 
             