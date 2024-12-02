#!/usr/bin/env -S dotnet fsi
// Copyright 2024 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2024-12-02
// See https://adventofcode.com/2024/day/1

open System

let parsePair (line:string) =
    let parts = line.Split("   ")
    int(parts.[0]), int(parts.[1])

let pairs = IO.File.ReadLines( fsi.CommandLineArgs.[1] ) |> Seq.map parsePair

let left = pairs |> Seq.map fst |> Seq.sort
let right = pairs |> Seq.map snd |> Seq.sort

let rightCounts = right
                    |> Seq.groupBy (fun v -> v)
                    |> Seq.map (fun (k, v) -> (k, List.ofSeq(v).Length))
                    |> Map.ofSeq

left |> Seq.map (fun v -> v * (Map.tryFind v rightCounts|> Option.defaultValue 0))
     |> Seq.sum
     |> printfn "%A"
