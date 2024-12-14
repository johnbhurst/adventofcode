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
let indices = seq { for i in 0..rows-1 do for j in 0..cols-1 do yield i, j }
let start = indices |> Seq.find (fun (i, j) -> grid.[i, j] = '^')

let rec moveUp position positions =
    let (i, j) = position
    if i = 0 then positions
    elif grid[i-1, j] = '#' then moveRight position positions
    else moveUp (i-1, j) (Set.add (i-1, j) positions)
and moveRight position positions =
    let (i, j) = position
    if j = cols-1 then positions
    elif grid[i, j+1] = '#' then moveDown position positions
    else moveRight (i, j+1) (Set.add (i, j+1) positions)
and moveDown position positions =
    let (i, j) = position
    if i = rows-1 then positions
    elif grid[i+1, j] = '#' then moveLeft position positions
    else moveDown (i+1, j) (Set.add (i+1, j) positions)
and moveLeft position positions =
    let (i, j) = position
    if j = 0 then positions
    elif grid[i, j-1] = '#' then moveUp position positions
    else moveLeft (i, j-1) (Set.add (i, j-1) positions)

moveUp start (Set.singleton start)
    |> Set.count
    |> printfn "%A"
