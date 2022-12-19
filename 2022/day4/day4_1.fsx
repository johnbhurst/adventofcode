#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-04

let parse (line:string) =
    let pair = line.Split(',')
    let pair1 = pair.[0].Split('-') |> Array.map int
    let pair2 = pair.[1].Split('-') |> Array.map int
    (pair1.[0], pair1.[1], pair2.[0], pair2.[1])

let score (a,b,c,d) = if a >= c && b <= d || c >= a && d <=  b then 1 else 0

System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
    |> List.ofSeq
    |> List.map (parse >> score)
    |> List.sum
    |> printfn "%A"
