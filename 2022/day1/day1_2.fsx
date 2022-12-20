#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-01
// See https://adventofcode.com/2022/day/1

let split (sep:string) (str:string) = str.Split(sep)

// group lines into lists of lines separated by blank lines
let groupLines lines =
    lines
    |> String.concat "#"
    |> split "##"
    |> Seq.map (split "#")

System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
    |> groupLines
    |> Seq.map (Seq.map int)
    |> Seq.map Seq.sum
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum
    |> printfn "%A"
