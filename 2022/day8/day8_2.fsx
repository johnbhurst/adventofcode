#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-20
// See https://adventofcode.com/2022/day/8

let rec countUntil predicate s =
    if Seq.isEmpty s then
        0
    else
        let head, tail = Seq.head s, Seq.tail s
        if predicate head then 1 else 1 + countUntil predicate tail

let heights = System.IO.File.ReadLines( fsi.CommandLineArgs.[1] ) |> array2D
let rows = Array2D.length1 heights
let cols = Array2D.length2 heights

let scores = seq {
    for i in 1 .. rows - 2 do
        for j in 1 .. cols - 2 do
            let colValueGreaterEqual x = heights[i,x] >= heights[i,j]
            let rowValueGreaterEqual x = heights[x,j] >= heights[i,j]
            let leftScore = countUntil colValueGreaterEqual (seq { j-1 .. -1 .. 0 })
            let rightScore = countUntil colValueGreaterEqual (seq { j+1 .. cols-1 })
            let topScore = countUntil rowValueGreaterEqual (seq { i-1 .. -1 .. 0 })
            let bottomScore = countUntil rowValueGreaterEqual (seq { i+1 .. rows-1 })
            yield leftScore * rightScore * topScore * bottomScore
    }
let result = Seq.max scores
printfn "%d" result
