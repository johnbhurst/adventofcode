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
let pairs = seq { for i in 0..rows-1 do for j in 0..cols-1 do yield i, j } |> List.ofSeq

let getstring i j ioffset joffset =
    [|grid[i, j]; grid[i+ioffset, j+joffset]; grid[i+2*ioffset, j+2*joffset]; grid[i+3*ioffset, j+3*joffset]|]
    |> System.String

let right i j     =                j <= cols-4 && getstring i j +0 +1 = "XMAS"
let downright i j = i <= rows-4 && j <= cols-4 && getstring i j +1 +1 = "XMAS"
let down i j      = i <= rows-4                && getstring i j +1 +0 = "XMAS"
let downleft i j  = i <= rows-4 && j >= 3      && getstring i j +1 -1 = "XMAS"
let left i j      =                j >= 3      && getstring i j +0 -1 = "XMAS"
let upleft i j    = i >= 3      && j >= 3      && getstring i j -1 -1 = "XMAS"
let up i j        = i >= 3                     && getstring i j -1 +0 = "XMAS"
let upright i j   = i >= 3      && j <= cols-4 && getstring i j -1 +1 = "XMAS"

[right; downright; down; downleft; left; upleft; up; upright]
    |> List.collect (fun test -> pairs |> List.filter (fun (i, j) -> test i j) )
    |> List.length
    |> printfn "%A"
