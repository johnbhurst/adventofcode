#!/usr/bin/env -S dotnet fsi
// Copyright 2023 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2023-12-01
// See https://adventofcode.com/2023/day/1

open System

let rec replaceDigits line =
    match List.ofSeq line with
    | 'o' :: 'n' :: 'e' :: rest -> '1' :: replaceDigits ('e' :: rest)
    | 't' :: 'w' :: 'o' :: rest -> '2' :: replaceDigits ('o' :: rest)
    | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: rest -> '3' :: replaceDigits ('e' :: rest)
    | 'f' :: 'o' :: 'u' :: 'r' :: rest -> '4' :: replaceDigits rest
    | 'f' :: 'i' :: 'v' :: 'e' :: rest -> '5' :: replaceDigits ('e' :: rest)
    | 's' :: 'i' :: 'x' :: rest -> '6' :: replaceDigits rest
    | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: rest -> '7' :: replaceDigits ('n' :: rest)
    | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: rest -> '8' :: replaceDigits ('t' :: rest)
    | 'n' :: 'i' :: 'n' :: 'e' :: rest -> '9' :: replaceDigits ('e' :: rest)
    | c :: rest -> c :: replaceDigits rest
    | [] -> []

let toDigit c = (int c) - (int '0')

let calibrationValue digits = 10 * Seq.head digits + Seq.last digits

System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
    |> Seq.map (List.ofSeq >> replaceDigits >> Seq.filter Char.IsDigit >> Seq.map toDigit >> calibrationValue)
    |> Seq.sum
    |> printfn "%A"
