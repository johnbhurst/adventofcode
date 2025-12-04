#!/usr/bin/env -S dotnet fsi
// Copyright 2025 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2025-12-04
// See https://adventofcode.com/2025/day/1

// L68
// L30
// R48
// L5
// R60
// L55
// L1
// L99
// R14
// L82

let offset (line:string): int =
    let sgn = if line.Substring(0,1) = "R" then 1 else -1
    sgn * int(line.Substring(1))

let mod100 n = ((n % 100) + 100) % 100

let newpos current offset = current + offset |> mod100

System.IO.File.ReadLines(fsi.CommandLineArgs[1])
    |> Seq.map offset
    |> Seq.scan newpos 50
    |> Seq.filter ((=) 0)
    |> Seq.length
    |> printfn "%A"
