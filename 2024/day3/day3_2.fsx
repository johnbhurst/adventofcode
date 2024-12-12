#!/usr/bin/env -S dotnet fsi
// Copyright 2024 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2024-12-03
// See https://adventofcode.com/2024/day/3

open System.Text.RegularExpressions

let input =
    if fsi.CommandLineArgs.Length > 1
        then System.IO.File.ReadLines(fsi.CommandLineArgs.[1])
        else ("""xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))""".Split('\n'))

let rec parseMul (s:string) =
    let m = Regex.Match(s, @"(don't\(\))(.*)|mul\((\d{1,3}),(\d{1,3})\)(.*)")
    if m.Success then
        if m.Groups.[1].Value = "don't()" then
            parseNoMul m.Groups.[2].Value
        else
            let a = int m.Groups.[3].Value
            let b = int m.Groups.[4].Value
            let rest = m.Groups.[5].Value
            a * b :: (parseMul rest)
    else
        []
and parseNoMul (s:string) =
    let m = Regex.Match(s, @"do\(\)(.*)")
    if m.Success then
        parseMul m.Groups.[1].Value
    else
        []

input
    |> List.ofSeq
    |> String.concat ""
    |> parseMul
    |> List.sum
    |> printfn "%A"