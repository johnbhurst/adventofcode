#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-29
// See https://adventofcode.com/2022/day/14

let parseSegments (line:string) : ((int*int)*(int*int)) list =
    let rec pairs (points: (int*int) list) : ((int*int)*(int*int)) list =
        match points with
        | a :: b :: rest -> (a,b) :: pairs (b :: rest)
        | _ -> []
    line.Split(" -> ") |> Array.map (fun s -> s.Split(",")) |> Array.map (fun a -> (int a.[1], int a.[0])) |> Array.toList |> pairs

let rockSegments = System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
                    |> Seq.toList
                    |> List.map parseSegments
                    |> List.concat

let maxRow = rockSegments |> List.map (fun ((row1, col1), (row2, col2)) -> max row1 row2) |> List.max
let maxCol = rockSegments |> List.map (fun ((row1, col1), (row2, col2)) -> max col1 col2) |> List.max
let cave = Array2D.zeroCreate<bool> (maxRow+1) (maxCol+1)

for ((row1, col1), (row2, col2)) in rockSegments do
    for row in (min row1 row2)..(max row1 row2) do
        for col in (min col1 col2)..(max col1 col2) do
            cave.[row,col] <- true

let rec findStopPos row col =
    if row = maxRow then None
    elif not cave.[row+1,col] then findStopPos (row+1) col
    elif not cave.[row+1,col-1] then findStopPos (row+1) (col-1)
    elif not cave.[row+1,col+1] then findStopPos (row+1) (col+1)
    else
        cave.[row,col] <- true
        Some (row, col)

// Each call to findStopPos returns the position a unit of sand will stop in, and marks that position filled.
// List.unfold results in a list of all positions filled by units of sand.
List.unfold (fun state -> findStopPos 0 500) 1
    |> List.length
    |> printfn "%d"
