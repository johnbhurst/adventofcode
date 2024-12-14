#!/usr/bin/env -S dotnet fsi
// Copyright 2024 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2024-12-01
// See https://adventofcode.com/2024/day/1

let input = System.IO.File.ReadLines(fsi.CommandLineArgs.[1])

let parsePair (line:string) =
    let parts = line.Split("   ")
    int(parts.[0]), int(parts.[1])

let distBetween (a:int, b:int) = System.Math.Abs(a - b)

let pairs = input |> Seq.map parsePair       // seq [(3, 4); (4, 3); (2, 5); (1, 3); (3, 9); (3, 3)]
let left = pairs |> Seq.map fst |> Seq.sort  // seq [1; 2; 3; 3; 3; 4]
let right = pairs |> Seq.map snd |> Seq.sort // seq [3; 3; 3; 4; 5; 9]

Seq.zip left right |> Seq.map distBetween    // seq [2; 1; 0; 1; 2; 5]
    |> Seq.sum                               // 11
    |> printfn "%A"
