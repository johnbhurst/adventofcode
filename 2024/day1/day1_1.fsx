#!/usr/bin/env -S dotnet fsi
// Copyright 2024 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2024-12-01
// See https://adventofcode.com/2024/day/1

open System

let parsePair (line:string) =
    let parts = line.Split("   ")
    parts.[0], parts.[1]

let distBetween (a:int, b:int) = Math.Abs(a - b)

let pairs = IO.File.ReadLines( fsi.CommandLineArgs.[1] ) |> Seq.map parsePair

let left = pairs |> Seq.map (fun (a, b) -> a |> int) |> Seq.sort
let right = pairs |> Seq.map (fun (a, b) -> b |> int) |> Seq.sort

Seq.zip left right |> Seq.map distBetween
    |> Seq.sum
    |> printfn "%A"
