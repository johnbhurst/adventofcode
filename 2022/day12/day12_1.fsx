#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-25
// See https://adventofcode.com/2022/day/12

let heightmap = System.IO.File.ReadLines( fsi.CommandLineArgs.[1] ) |> Array.ofSeq

let findPos c = seq { for row in 0 .. heightmap.Length - 1 do
                         for col in 0 .. heightmap.[row].Length - 1 do
                            if heightmap.[row].[col] = c then
                                yield (row, col) } |> Seq.head

let startPos = findPos 'S'
let endPos = findPos 'E'

let eligibleMoves (row, col) =
    let heightOk (row2, col2) =
        let stdheight (x:char) = if x = 'S' then 0 elif x = 'E' then int 'z' - int 'a' else int x - int 'a'
        let height1 = heightmap.[row].[col] |> stdheight
        let height2 = heightmap.[row2].[col2] |> stdheight
        height2 - height1 <= 1 || height2 < height1
    seq { if row > 0 && heightOk (row - 1, col) then yield (row - 1, col)
          if row < heightmap.Length - 1 && heightOk (row + 1, col) then yield (row + 1, col)
          if col > 0 && heightOk (row, col - 1) then yield (row, col - 1)
          if col < heightmap.[row].Length - 1 && heightOk (row, col + 1) then yield (row, col + 1) }
        |> Seq.toList

let inline posEq (row, col) ((row2, col2), _) = row = row2 && col = col2

let rec findPaths visited positions =
    match positions with
    | [] -> visited
    | ((row, col), dist) :: restPositions ->
        if List.exists (posEq (row, col)) visited then findPaths visited restPositions
        else
            let notVisited pos = List.exists (posEq pos) visited |> not
            let notPositions pos = List.exists (posEq pos) positions |> not
            let otherPositions = eligibleMoves (row, col)
                                |> List.filter notVisited
                                |> List.filter notPositions
                                |> List.map (fun pos -> (pos, dist + 1))
            let newPositions = restPositions @ otherPositions
            let newVisited = visited @ [ (row, col), dist ]
            findPaths newVisited newPositions

findPaths [] [ (startPos, 0) ]
    |> List.filter (posEq endPos)
    |> List.map snd
    |> List.min
    |> printfn "%d"
