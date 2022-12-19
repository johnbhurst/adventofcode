#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-03
//See https://adventofcode.com/2022/day/2

let score line =
    match line with
    | "A X" -> 3 + 0
    | "A Y" -> 1 + 3
    | "A Z" -> 2 + 6
    | "B X" -> 1 + 0
    | "B Y" -> 2 + 3
    | "B Z" -> 3 + 6
    | "C X" -> 2 + 0
    | "C Y" -> 3 + 3
    | "C Z" -> 1 + 6
    | _ -> 0

System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
    |> List.ofSeq
    |> List.map score
    |> List.sum
    |> printfn "%A"
