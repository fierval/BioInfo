open System.Linq
open System.IO

#load "3g-EulerPath.fsx"
#load "3d-3e-debruin.fsx"

open ``3g-EulerPath``
open ``3d-3e-debruin``

let name = "rosalind_ba3h.txt"

let toString (cycle : string seq) =
    cycle.Skip(1) |> Seq.fold (fun acc e -> acc + e.Last().ToString()) (cycle.First())

let solve name =
    let sol = (File.ReadAllLines name).[1..] |> debruijn |> findPath |> toString
    File.WriteAllText(@"c:\temp\genome.txt", sol)
