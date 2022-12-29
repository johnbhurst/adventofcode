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

let row = int fsi.CommandLineArgs.[2]

let taxiDistance p1 p2 =
    abs (p1.X - p2.X) + abs (p1.Y - p2.Y)

let rowRange report =
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
        if end1 >= start2 then union ((start1, max end1 end2) :: rest)
        else (start1, end1) :: union ((start2, end2) :: rest)
    | range -> range

let ranges =
    reports
    |> List.map rowRange
    |> List.choose id
    |> List.sort
    |> union

let rangeSizes =
    ranges
    |> List.map (fun (start, end_) -> end_ - start + 1)
    |> List.sum

let beaconsInRanges =
    reports
    |> List.map (fun report -> report.Beacon)
    |> List.filter (fun beacon -> beacon.Y = row && ranges |> List.exists (fun (start, end_) -> start <= beacon.X && beacon.X <= end_))
    |> List.distinct
    |> List.length

printfn "%d" (rangeSizes - beaconsInRanges)
