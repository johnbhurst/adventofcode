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

let rec chunk40 (str:string) =
    if str.Length <= 40 then
        str
    else
        str.[0..39] + "\n" + (chunk40 str.[40..])

let signals = System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
                |> Seq.map parse
                |> Seq.concat
                |> Seq.scan (+) 1
                |> Seq.take 240

let positions = seq { 0 .. 239 }
                |> Seq.map (fun i -> i % 40)

let pixel (position, signal) = if abs(position - signal) <= 1 then "#" else "."

Seq.zip positions signals
|> Seq.map pixel
|> String.concat ""
|> chunk40
|> printfn "%s"