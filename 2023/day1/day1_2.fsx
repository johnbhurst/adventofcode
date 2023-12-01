#!/usr/bin/env -S dotnet fsi
// Copyright 2023 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2023-12-01
// See https://adventofcode.com/2023/day/1

open System

let rec repl (line:list<char>) =
    match List.ofSeq line with
    | 'o' :: 'n' :: 'e' :: rest -> '1' :: repl ('e' :: rest)
    | 't' :: 'w' :: 'o' :: rest -> '2' :: repl ('o' :: rest)
    | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: rest -> '3' :: repl ('e' :: rest)
    | 'f' :: 'o' :: 'u' :: 'r' :: rest -> '4' :: repl rest
    | 'f' :: 'i' :: 'v' :: 'e' :: rest -> '5' :: repl ('e' :: rest)
    | 's' :: 'i' :: 'x' :: rest -> '6' :: repl rest
    | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: rest -> '7' :: repl ('n' :: rest)
    | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: rest -> '8' :: repl ('t' :: rest)
    | 'n' :: 'i' :: 'n' :: 'e' :: rest -> '9' :: repl ('e' :: rest)
    // | 'z' :: 'e' :: 'r' :: 'o' :: rest -> '0' :: repl rest
    | c :: rest -> c :: repl rest
    | [] -> []

let dig c = (int c) - (int '0')

System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
    |> Seq.map List.ofSeq
    |> Seq.map repl
    |> Seq.map (fun line -> Seq.filter (fun c -> Char.IsDigit c) line)
    |> Seq.map (fun line -> Seq.map dig line)
    |> Seq.map List.ofSeq
    |> Seq.map (fun list -> 10 * List.head list + List.last list)
    |> Seq.sum
    |> printfn "%A"
