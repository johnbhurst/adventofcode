#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-21
// See https://adventofcode.com/2022/day/10

let parse (line:string) =
    if line.StartsWith("addx") then
        let n = int line[5..]
        [0; n]
    elif line = "noop" then
        [0]
    else
        failwithf "Unknown instruction: %s" line

let registerValues = System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
                        |> Seq.map parse
                        |> List.ofSeq
                        |> List.concat
                        |> List.scan (+) 1
let signalStrength i = i * registerValues.[i-1]

seq { 20 .. 40 .. List.length registerValues }
    |> Seq.map signalStrength
    |> Seq.sum
    |> printfn "%d"
