#!/usr/bin/env -S dotnet fsi
// Copyright 2023 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2023-12-20
// See https://adventofcode.com/2023/day/4

open System.Text.RegularExpressions

let parseNums line =
    let m = Regex.Match(line, @"^Card +\d+: (.*) \| (.*)$")
    if m.Success then
        let winningNums = Regex(@" +").Split(m.Groups.[1].Value.Trim()) |> Seq.map int
        let playerNums = Regex(@" +").Split(m.Groups.[2].Value.Trim()) |> Seq.map int
        (winningNums, playerNums)
    else
        ([], [])

let rec score n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> 2 * score (n-1)

System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
    |> Seq.map parseNums
    |> Seq.map (fun (winningNums, playerNums) -> Seq.filter (fun n -> winningNums |> Seq.contains n) playerNums)
    |> Seq.map Seq.length
    |> Seq.map score
    |> Seq.sum
    |> printfn "%d"
