#!/usr/bin/env -S dotnet fsi
// Copyright 2023 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2023-12-01
// See https://adventofcode.com/2023/day/1

open System

let dig c = (int c) - (int '0')

System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
    |> Seq.map (fun line -> Seq.filter (fun c -> Char.IsDigit c) line)
    |> Seq.map (fun line -> Seq.map dig line)
    |> Seq.map List.ofSeq
    |> Seq.map (fun list -> 10 * List.head list + List.last list)
    |> Seq.sum
    |> printfn "%A"
