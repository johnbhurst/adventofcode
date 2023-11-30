#!/usr/bin/env -S dotnet fsi
// Copyright 2023 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2023-01-01
// See https://adventofcode.com/2022/day/16

open System.Text.RegularExpressions

type Valve = { Name: string; Rate: int; Tunnels: string list }

let parse line =
    let m = Regex.Match(line, @"Valve ([A-Z]+) has flow rate=(\d+); tunnel(?:s)? lead(?:s)? to valve(?:s)? ([A-Z]+(?:, [A-Z]+)*)")
    if m.Success then
        { Name = m.Groups.[1].Value; Rate = int m.Groups.[2].Value; Tunnels = m.Groups.[3].Value.Split(", ") |> List.ofArray }
    else
        failwithf "Invalid line: %s" line

let valves =
    System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
    |> Seq.map parse
    |> Seq.map (fun v -> (v.Name, v))
    |> Map.ofSeq

let rec reachable (valve:string) (visited:string list) : (string*int) list =
    let nextReachable neighbour =
        if valves.[neighbour].Rate > 0
            then [(neighbour,1)]
            else reachable neighbour (neighbour :: visited)
                |> List.map (fun (n,d) -> (n,d+1))
    valves.[valve].Tunnels
        |> List.filter (fun n -> not (List.contains n visited)) |> List.map nextReachable |> List.concat

let reachableMap =
    valves
    |> Map.keys
    |> Seq.map (fun valve -> (valve, reachable valve [valve]))
    |> Map.ofSeq

let rec bestScore (valve:string) (minutes:int) (opened:string list) (sinceOpening:string list) : int * string list =
    if minutes <= 2 then 0, []
    else
        let neighbours = reachableMap.[valve]
                            |> List.filter (fun (n,d) -> not (List.contains n sinceOpening))
        if neighbours = [] then 0, []
        else
            let skipResults = neighbours |> List.map (fun (n,d) -> bestScore n (minutes - d) opened (n :: sinceOpening))
            if List.contains valve opened then
                let bestScore, bestPath = skipResults |> List.maxBy fst
                bestScore, valve :: bestPath
            else
                let thisScore = (minutes-2)*valves.[valve].Rate
                let openResults = neighbours
                                |> List.map (fun (n,d) -> bestScore n (minutes - d - 1) (valve :: opened) [n])
                                |> List.map (fun (s,p) -> s+thisScore, p)
                let bestScore, bestPath = skipResults @ openResults |> List.maxBy fst
                bestScore, valve :: bestPath

bestScore "AA" 31 [] [] |> printfn "%A"
