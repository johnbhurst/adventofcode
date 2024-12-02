#!/usr/bin/env -S dotnet fsi
// Copyright 2024 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2024-12-02
// See https://adventofcode.com/2024/day/2

open System

let parseData (line:string) =
    line.Split(" ")
        |> Seq.map int
        |> List.ofSeq

let rec safeAscending data =
    match data with
    | h1 :: h2 :: rest -> h1 < h2 && h2 - h1 <= 3 && safeAscending (h2::rest)
    | _ -> true

let rec safeDescending data =
    match data with
    | h1 :: h2 :: rest -> h1 > h2 && h1 - h2 <= 3 && safeDescending (h2::rest)
    | _ -> true

let safe data =
    match data with
    | h1 :: h2 :: rest -> if h1 < h2 then safeAscending (h1::h2::rest)
                          else if h1 > h2 then safeDescending (h1::h2::rest)
                               else false
    | _ -> false

IO.File.ReadLines( fsi.CommandLineArgs.[1] )
    |> Seq.map parseData  // seq { [1; 2; 3]; [4; 5; 6]; ... }
    |> Seq.map safe       // seq { true; false; ... }
    |> Seq.filter id      // seq { true; ... }
    |> Seq.length         // int
    |> printfn "%A"
