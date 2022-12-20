#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-21
// See https://adventofcode.com/2022/day/9

let parse (line:string) =
    let parts = line.Split(' ')
    let direction = parts.[0]
    let distance = int parts.[1]
    match direction with
    | "U" -> seq { for i in 1..distance -> (0, 1) } |> List.ofSeq
    | "D" -> seq { for i in 1..distance -> (0, -1) } |> List.ofSeq
    | "L" -> seq { for i in 1..distance -> (-1, 0) } |> List.ofSeq
    | "R" -> seq { for i in 1..distance -> (1, 0) } |> List.ofSeq
    | _ -> failwithf "Unknown direction: %s" direction

let moveHead (x, y) (dx, dy) = (x + dx, y + dy)

let moveTail (x, y) (hx, hy) =
    if x = hx then
        if y > hy + 1 then (x, y - 1)
        elif y < hy - 1 then (x, y + 1)
        else (x, y)
    elif y = hy then
        if x > hx + 1 then (x - 1, y)
        elif x < hx - 1 then (x + 1, y)
        else (x, y)
    elif abs(x-hx) + abs(y-hy) > 2 then
        if x > hx && y > hy then (x - 1, y - 1)
        elif x > hx && y < hy then (x - 1, y + 1)
        elif x < hx && y > hy then (x + 1, y - 1)
        else (x + 1, y + 1)
    else (x, y)

let applyMovement knots (dx, dy) =
    let rec apply1 newHead tail (dx, dy) =
        match tail with
        | [] -> [newHead]
        | (x, y) :: tail' ->
            let newTail = moveTail (x, y) newHead
            newHead :: apply1 newTail tail' (dx, dy)
    match knots with
    | [] -> []
    | head :: tail ->
        let newHead = moveHead head (dx, dy)
        apply1 newHead tail (dx, dy)

let startingKnots = List.replicate 10 (0, 0)

System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
    |> Seq.map parse
    |> List.ofSeq
    |> List.concat
    |> List.scan applyMovement startingKnots
    |> List.map List.last
    |> List.distinct
    |> List.length
    |> printfn "%A"
