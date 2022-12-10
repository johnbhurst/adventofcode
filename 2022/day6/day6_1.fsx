#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-11

let rec packetPos p l =
    match l with
    | a :: b :: c :: d :: rest -> if a <> b && a <> c && a <> d && b <> c && b <> d && c <> d then p+4 else packetPos (p+1) (b :: c :: d :: rest)
    | _ -> failwith "packetPos: invalid list"

let result = System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
                |> List.ofSeq
                |> List.head
                |> Seq.toList
                |> packetPos 0

printfn "%A" result