#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-04

let rec partition l =
    match l with
    | h1 :: h2 :: h3 :: t -> (h1, h2, h3) :: partition t
    | _ -> []

let finddup (s1, s2, s3) =
    let l1 = Seq.toList s1
    let l2 = Seq.toList s2
    let l3 = Seq.toList s3
    let contains c = List.exists (fun e -> e = c)
    List.find (fun c -> contains c l2 && contains c l3) l1

let score c =
    match c with
    | t when t >= 'a' && t <= 'z' -> 1 + (int t - int 'a')
    | t when t >= 'A' && t <= 'Z' -> 27 + (int t - int 'A')
    | _ -> 0

let result = System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
                |> List.ofSeq
                |> partition
                |> List.map (finddup >> score)
                |> List.sum

printf "%A\n" result
