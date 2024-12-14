#!/usr/bin/env -S dotnet fsi
// Copyright 2024 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2024-12-03
// See https://adventofcode.com/2024/day/3

open System.Text.RegularExpressions

let input = System.IO.File.ReadLines(fsi.CommandLineArgs[1])

let rec parseMul (s:string) =
    let m = Regex.Match(s, @"mul\((\d{1,3}),(\d{1,3})\)(.*)")
    if m.Success then
        let a = int m.Groups[1].Value
        let b = int m.Groups[2].Value
        let rest = m.Groups[3].Value
        a * b :: (parseMul rest)
    else
        []

input
    |> List.ofSeq
    |> String.concat ""
    |> parseMul
    |> List.sum
    |> printfn "%A"
