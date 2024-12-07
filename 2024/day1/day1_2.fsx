#!/usr/bin/env -S dotnet fsi
// Copyright 2024 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2024-12-02
// See https://adventofcode.com/2024/day/1

let input =
    if fsi.CommandLineArgs.Length > 1
        then System.IO.File.ReadLines(fsi.CommandLineArgs.[1])
        else ("""3   4
4   3
2   5
1   3
3   9
3   3""".Split('\n'))

let parsePair (line:string) =
    let parts = line.Split("   ")
    int(parts.[0]), int(parts.[1])

let pairs = input |> Seq.map parsePair       // seq [(3, 4); (4, 3); (2, 5); (1, 3); (3, 9); (3, 3)]
let left = pairs |> Seq.map fst |> Seq.sort  // seq [1; 2; 3; 3; 3; 4]
let right = pairs |> Seq.map snd |> Seq.sort // seq [3; 3; 3; 4; 5; 9]

let rightCounts =
    right
    |> Seq.groupBy (fun v -> v)                          // seq [(3, seq [3; 3; 3]); (4, seq [4]); (5, seq [5]); (9, seq [9])]
    |> Seq.map (fun (k, v) -> (k, List.ofSeq(v).Length)) // seq [(3, 3); (4, 1); (5, 1); (9, 1)]
    |> Map.ofSeq                                         // map [(3, 3); (4, 1); (5, 1); (9, 1)]

left |> Seq.map (fun v -> v * (Map.tryFind v rightCounts|> Option.defaultValue 0)) // seq [0; 0; 9; 9; 9; 4]
     |> Seq.sum                                                                    // 31
     |> printfn "%A"
