#!/usr/bin/env -S dotnet fsi
// Copyright 2024 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2024-12-13
// See https://adventofcode.com/2024/day/4

let input =
    if fsi.CommandLineArgs.Length > 1
        then System.IO.File.ReadLines(fsi.CommandLineArgs.[1])
        else ("""MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX""".Split('\n'))

let grid = array2D input
// [['M'; 'M'; 'M'; 'S'; 'X'; 'X'; 'M'; 'A'; 'S'; 'M']
//  ['M'; 'S'; 'A'; 'M'; 'X'; 'M'; 'S'; 'M'; 'S'; 'A']
//  ['A'; 'M'; 'X'; 'S'; 'X'; 'M'; 'A'; 'A'; 'M'; 'M']
//  ['M'; 'S'; 'A'; 'M'; 'A'; 'S'; 'M'; 'S'; 'M'; 'X']
//  ['X'; 'M'; 'A'; 'S'; 'A'; 'M'; 'X'; 'A'; 'M'; 'M']
//  ['X'; 'X'; 'A'; 'M'; 'M'; 'X'; 'X'; 'A'; 'M'; 'A']
//  ['S'; 'M'; 'S'; 'M'; 'S'; 'A'; 'S'; 'X'; 'S'; 'S']
//  ['S'; 'A'; 'X'; 'A'; 'M'; 'A'; 'S'; 'A'; 'A'; 'A']
//  ['M'; 'A'; 'M'; 'M'; 'M'; 'X'; 'M'; 'M'; 'M'; 'M']
//  ['M'; 'X'; 'M'; 'X'; 'A'; 'X'; 'M'; 'A'; 'S'; 'X']]
// grid |> printfn "%A"

let rows = Array2D.length1 grid
let cols = Array2D.length2 grid
let pairs = seq { for i in 1..rows-2 do for j in 1..cols-2 do yield i, j } |> List.ofSeq

let xmas (i, j) =
    grid[i, j] = 'A'
      && (grid[i-1,j-1] = 'M' && grid[i+1,j+1] = 'S' || grid[i-1,j-1] = 'S' && grid[i+1,j+1] = 'M')
      && (grid[i-1,j+1] = 'M' && grid[i+1,j-1] = 'S' || grid[i-1,j+1] = 'S' && grid[i+1,j-1] = 'M')

pairs
    |> List.filter xmas
    |> List.length
    |> printfn "%A"
