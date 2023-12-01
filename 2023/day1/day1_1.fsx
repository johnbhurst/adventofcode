#!/usr/bin/env -S dotnet fsi
// Copyright 2023 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2023-12-01
// See https://adventofcode.com/2023/day/1

open System

let toDigit c = (int c) - (int '0')

let calibrationValue digits = 10 * List.head digits + List.last digits

System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
    |> Seq.map ((Seq.filter Char.IsDigit) >> (Seq.map toDigit) >> List.ofSeq >> calibrationValue)
    |> Seq.sum
    |> printfn "%A"
