#!/usr/bin/env -S dotnet fsi
// Copyright 2024 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2024-12-14
// See https://adventofcode.com/2024/day/6

let input =
    if fsi.CommandLineArgs.Length > 1
        then System.IO.File.ReadLines(fsi.CommandLineArgs.[1])
        else ("""....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...""".Split('\n'))

let grid = array2D input
let rows = Array2D.length1 grid
let cols = Array2D.length2 grid
let indices = seq { for i in 0..rows-1 do for j in 0..cols-1 -> i, j }
let start = indices |> Seq.find (fun (i, j) -> grid.[i, j] = '^')

let diroffset n =
    match n with
    | 0 -> -1, 0
    | 1 -> 0, 1
    | 2 -> 1, 0
    | 3 -> 0, -1
    | _ -> 0, 0

let rec path direction position positions =
    let (i, j) = position
    let (ioffset, joffset) = diroffset direction
    let (i2, j2) = i + ioffset, j + joffset
    if i2 < 0 || i2 >= rows || j2 < 0 || j2 >= cols then positions
    elif grid.[i2, j2] = '#' then path ((direction + 1) % 4) position positions
    else path direction (i2, j2) (Set.add (i2, j2) positions)

path 0 start (Set.singleton start)
    |> Set.count
    |> printfn "%A"
