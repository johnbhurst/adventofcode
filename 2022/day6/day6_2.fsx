#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-11
// See https://adventofcode.com/2022/day/6

let nodups l =
    let rec dup l =
        match l with
        | a :: b :: rest -> if a = b then true else dup (b :: rest)
        | _ -> false
    not (dup (List.sort l))

let rec packetPos p l =
    let head14 = l |> List.take 14
    if nodups head14 then p+14 else packetPos (p+1) (List.tail l)

System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
    |> List.ofSeq
    |> List.head
    |> Seq.toList
    |> packetPos 0
    |> printfn "%A"
