#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-20
// See https://adventofcode.com/2022/day/8

let heights = System.IO.File.ReadLines( fsi.CommandLineArgs.[1] ) |> array2D
let rows = Array2D.length1 heights
let cols = Array2D.length2 heights
let outsideCount = 2 * rows + 2 * cols - 4
let insideCount = Seq.sum (seq {
    for i in 1..rows-2 do
        for j in 1..cols-2 do
            let value = heights[i,j]
            let less = fun x -> x < value
            if Array.forall less heights[i,0..j-1] || Array.forall less heights[i,j+1..cols-1] ||
               Array.forall less heights[0..i-1,j] || Array.forall less heights[i+1..rows-1,j] then
                yield 1
    } )
let result = outsideCount + insideCount
printfn "%d" result
