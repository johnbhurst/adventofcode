#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-01

let rec splitlist sep xs =
    match xs with
    | [] -> []
    | h :: t -> if (h = sep) then [] :: splitlist sep t else
                    match splitlist sep t with
                    | [] -> [[h]]
                    | h2 :: t2 -> (h :: h2) :: t2

System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
    |> List.ofSeq
    |> splitlist ""
    |> List.map (fun xs -> xs |> List.map int |> List.sum)
    |> List.max
    |> printfn "%A"
