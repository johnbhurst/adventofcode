#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-30
// See https://adventofcode.com/2022/day/15

open System.Text.RegularExpressions

type Point = { X: int; Y: int }
type Report = { Sensor: Point; Beacon: Point }

let parseReport line =
    let m = Regex.Match(line, @"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)")
    if m.Success then
        { Sensor = { X = int m.Groups.[1].Value; Y = int m.Groups.[2].Value }; Beacon = { X = int m.Groups.[3].Value; Y = int m.Groups.[4].Value}}
    else
        failwithf "Invalid report [%s]" line

let reports = System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
                |> Seq.toList
                |> List.map parseReport

let maxRow = int fsi.CommandLineArgs.[2]

let taxiDistance p1 p2 =
    abs (p1.X - p2.X) + abs (p1.Y - p2.Y)

let rowRange row report =
    let dist = taxiDistance report.Sensor report.Beacon
    let rowDist = abs (row - report.Sensor.Y)
    if rowDist > dist then
        None
    else
        let colDist = dist - rowDist
        Some (report.Sensor.X - colDist, report.Sensor.X + colDist)

let rec union rangeList =
    match rangeList with
    | [] -> []
    | (start1, end1) :: (start2, end2) :: rest ->
        if end1 + 1 >= start2 then union ((start1, max end1 end2) :: rest)
        else (start1, end1) :: union ((start2, end2) :: rest)
    | range -> range

let ranges row =
    reports
    |> List.map (rowRange row)
    |> List.choose id
    |> List.sort
    |> union

let unions =
    [ for row in 0 .. maxRow -> ranges row ]
    |> List.mapi (fun row ranges -> row, ranges)
    |> List.filter (fun item -> List.length (snd item) > 1)

let item = List.head unions

let row = bigint (fst item)
let col = bigint ((snd item |> List.head |> snd) + 1)
let result = row + (bigint 4000000) * col

printfn "%A" result
