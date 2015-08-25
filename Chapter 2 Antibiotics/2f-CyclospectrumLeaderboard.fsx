open System
open System.Linq
open System.Collections.Generic
open System.IO

#load "2e-Cyclospectrum.fsx"

open ``2e-Cyclospectrum``
open ``2a-2c``

/// spectrum must be an ordered array, n - top n entries on the leaderboard with ties
let cyclopeptydeAlphabet (alphabet : int []) (spectrum : int []) n =
    if spectrum = Unchecked.defaultof<int []> || spectrum.Length = 0 then failwith "Empty spectrum"
    let mass = spectrum.Last()
    let leaderPeptide = List<int>()
    leaderPeptide.Add(alphabet.[0])

    // convert spectrum to the dictionary of mass -> #times occuring
    let specDict = dictOfAminos spectrum

    /// number of peptides shared between cyclospectrums of current peptide and the given spectrum
    let score (peptide : int seq)  =
        let cyclospec = cyclopsecArr (peptide |> Seq.toArray)
        let cycloDict = dictOfAminos cyclospec
        let keys = specDict.Keys.Intersect(cyclospec)
        keys 
        |> Seq.map (fun k -> if cycloDict.[k] <= specDict.[k] then cycloDict.[k] else specDict.[k])
        |> Seq.sum

    // cut the leaderboard to have only n items with ties
    let cut (board : List<List<int>>) n =
        board
            |> Seq.groupBy (fun e -> score e)
            |> Seq.sortBy (fun (k, sq) -> -k)
            |> Seq.fold (fun (state : List<List<int>>) (k, gr) -> (if state.Count < n then state.AddRange(gr)); state) (List<List<int>>())

    // create lists with an added peptide
    let expand (lst : List<List<int>>) =
        let newlst = List<List<int>>()
        for l in lst do
            for w in alphabet do
                let newl = List<int>()
                newl.AddRange(l)
                newl.Add w
                newlst.Add newl
        newlst

    // trim the list.
    // if mass of an element = mass of the spectrum - see if it qualifies for the output.
    // for the rest - make sure they are an exact subset of the spectrum
    let bound (lst : List<List<int>>) =

        // determine the new leader
        let candidateOutput = lst |> Seq.filter (fun l -> l |> Seq.sum = mass)
        if candidateOutput.Any() then
            let candidateLeader = candidateOutput |> Seq.maxBy (fun s -> score s)
            if score candidateLeader > score leaderPeptide then
                leaderPeptide.Clear()
                leaderPeptide.AddRange(candidateLeader)

        // trim the rest of the list, so each list is a subset of the spectrum
        let toRemove = lst.Where(fun l -> l.Sum() > mass)
        let rest = lst.Except(toRemove).ToList()
        if rest.Any() then
            cut rest n
        else
            rest
        
    let rec branchAndBound (lst : List<List<int>>) =
        if lst.Count = 0 then leaderPeptide
        else
            let lst = expand lst
            printfn "%d" lst.Count
            let rest = bound lst
            branchAndBound rest
    
    let leaderboard = List<List<int>>()
    Array.ForEach(alphabet, (fun w -> leaderboard.Add(List<int>()); leaderboard.Last().Add w))
    let leaderboard = cut leaderboard n

    let outStr = branchAndBound leaderboard
    
    // this is due to the weird format Rosalind wants: 186-128-113 for instance
    intSeqToRosalindWeights outStr

let spectrum = parseSpectrum "0 71 71 71 87 97 97 99 101 103 113 113 114 115 128 128 129 137 147 163 163 170 184 184 186 186 190 211 215 226 226 229 231 238 241 244 246 257 257 276 277 278 299 300 312 316 317 318 318 323 328 340 343 344 347 349 356 366 370 373 374 391 401 414 414 415 419 427 427 431 437 441 446 453 462 462 462 470 472 502 503 503 511 515 529 530 533 533 540 543 547 556 559 569 574 575 584 590 600 600 604 612 616 617 630 640 640 643 646 648 660 671 683 684 687 693 703 703 719 719 719 729 730 731 737 740 741 745 747 754 774 780 784 790 797 800 806 818 826 827 832 833 838 846 846 847 850 868 869 877 884 889 893 897 903 908 913 917 930 940 947 956 960 960 961 964 965 966 983 983 985 1002 1009 1010 1011 1021 1031 1031 1036 1053 1054 1058 1059 1062 1063 1074 1076 1084 1092 1103 1113 1122 1124 1130 1133 1134 1145 1146 1146 1149 1150 1155 1156 1171 1173 1174 1187 1191 1193 1200 1212 1221 1233 1240 1242 1246 1259 1260 1262 1277 1278 1283 1284 1287 1287 1288 1299 1300 1303 1309 1311 1320 1330 1341 1349 1357 1359 1370 1371 1374 1375 1379 1380 1397 1402 1402 1412 1422 1423 1424 1431 1448 1450 1450 1467 1468 1469 1472 1473 1473 1477 1486 1493 1503 1516 1520 1525 1530 1536 1540 1544 1549 1556 1564 1565 1583 1586 1587 1587 1595 1600 1601 1606 1607 1615 1627 1633 1636 1643 1649 1653 1659 1679 1686 1688 1692 1693 1696 1702 1703 1704 1714 1714 1714 1730 1730 1740 1746 1749 1750 1762 1773 1785 1787 1790 1793 1793 1803 1816 1817 1821 1829 1833 1833 1843 1849 1858 1859 1864 1877 1886 1890 1893 1900 1900 1903 1904 1918 1922 1930 1930 1931 1961 1963 1971 1971 1971 1980 1987 1992 1996 2002 2006 2006 2014 2018 2019 2019 2032 2042 2059 2060 2063 2067 2077 2084 2086 2089 2090 2093 2105 2110 2115 2115 2116 2117 2121 2133 2134 2155 2156 2157 2176 2176 2187 2189 2192 2195 2202 2204 2207 2207 2218 2222 2243 2247 2247 2249 2249 2263 2270 2270 2286 2296 2304 2305 2305 2318 2319 2320 2320 2330 2332 2334 2336 2336 2346 2362 2362 2362 2433"
let n = 325

let cyclopeptydeLeaderboard = cyclopeptydeAlphabet weights

let solve name =
    let lines = File.ReadAllLines name
    let n = int lines.[0]
    let spectrum = parseSpectrum lines.[1]
    cyclopeptydeLeaderboard spectrum n

let compareSolutions (s1 : string) (s2 : string) =
    let sol1 = s1.Split('-') |> Array.map int |> Array.sort
    let sol2 = s2.Split('-') |> Array.map int |> Array.sort
    sol1 = sol2