#!/usr/bin/env -S dotnet fsi
// Copyright 2023 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2023-12-20
// See https://adventofcode.com/2023/day/3

open System
open System.Text.RegularExpressions

type Num = { num: string; row: int; col: int }

// extract numbers and positions from an input line
let rec matchNums row offset line =
    let m = Regex.Match(line, @"^([^\d]*)(\d+)(.*)$")
    if m.Success then
        let col = offset + String.length m.Groups.[1].Value
        let num = m.Groups.[2].Value
        let rest = m.Groups.[3].Value
        { num = num; row = row; col = col } :: matchNums row (col + String.length num) rest
    else
        []

// check if a number is adjacent to a symbol
let adjacentSymbol lines num =
    let isValid (row, col) =
        row >= 0 && row < Array.length lines && col >= 0 && col < String.length lines.[row]
    let isSymbol (row, col) =
        let c = lines.[row].[col]
        not (Char.IsDigit c) && not (c = '.')
    seq { for row in num.row - 1 .. num.row + 1 do
            for col in num.col - 1 .. num.col + String.length num.num do
                yield ( row, col ) }
                |> Seq.filter isValid
                |> Seq.exists isSymbol

let lines = System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
let adjacentFilter = adjacentSymbol (Array.ofSeq lines)
lines
    |> Seq.mapi (fun row line -> matchNums row 0 line)
    |> Seq.concat
    |> Seq.filter adjacentFilter
    |> Seq.map (fun num -> num.num)
    |> Seq.sumBy int
    |> printfn "%A"
