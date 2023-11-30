#!/usr/bin/env -S dotnet fsi
// Copyright 2023 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2023-02-06
// See https://adventofcode.com/2022/day/17

type LeftRight = Left | Right

let cycle s = Seq.initInfinite (fun _ -> s) |> Seq.concat

let gasJets =
    System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
    |> Seq.head
    |> Seq.map (fun s -> if s = '<' then Left else Right)
    |> cycle
    |> Seq.take 50

gasJets |> Seq.iter (fun x -> printfn "%A" x)

