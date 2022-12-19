#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-04

let split (s:string) = s.Substring(0, s.Length / 2), s.Substring(s.Length / 2)

let finddup (s1, s2) =
    let l1 = Seq.toList s1
    let l2 = Seq.toList s2
    List.find (fun c -> List.exists (fun c2 -> c2=c) l2) l1

let score c =
    match c with
    | t when t >= 'a' && t <= 'z' -> 1 + (int t - int 'a')
    | t when t >= 'A' && t <= 'Z' -> 27 + (int t - int 'A')
    | _ -> 0

System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
    |> List.ofSeq
    |> List.map (split >> finddup >> score)
    |> List.sum
    |> printfn "%A"
