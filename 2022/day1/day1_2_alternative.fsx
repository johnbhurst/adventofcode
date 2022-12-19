#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-01

System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
    |> List.ofSeq
    |> String.concat "#"
    |> (fun s -> s.Split("##"))
    |> List.ofArray
    |> List.map (fun s -> s.Split('#') |> List.ofArray)
    |> List.map (fun l -> l |> List.map int)
    |> List.map List.sum
    |> List.sortBy (fun x -> -x)
    |> List.take 3
    |> List.sum
    |> printfn "%A"
