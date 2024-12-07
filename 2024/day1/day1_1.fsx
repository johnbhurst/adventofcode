#!/usr/bin/env -S dotnet fsi
// Copyright 2024 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2024-12-01
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

let distBetween (a:int, b:int) = System.Math.Abs(a - b)

let pairs = input |> Seq.map parsePair
let left = pairs |> Seq.map fst |> Seq.sort
let right = pairs |> Seq.map snd |> Seq.sort

Seq.zip left right |> Seq.map distBetween
    |> Seq.sum
    |> printfn "%A"
