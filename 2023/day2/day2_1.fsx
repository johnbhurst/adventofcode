#!/usr/bin/env -S dotnet fsi
// Copyright 2023 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2023-12-03
// See https://adventofcode.com/2023/day/2

open System.Text.RegularExpressions

type Combo = { Red: int; Green: int; Blue: int}
type Game = { ID: int; Combos: Combo list }

let parseColor s =
    let m = Regex.Match(s, "([0-9]+) ([a-z]+)")
    if m.Success then
        let count = int m.Groups.[1].Value
        let color = m.Groups.[2].Value
        color, count
    else
        failwithf "Cannot parse color from '%s'" s

let parseCombo (s:string) =
    let m = (s.Split(", ")) |> Seq.map parseColor  |> Map.ofSeq
    { Red = Map.tryFind "red" m |> Option.defaultValue 0;
        Green = Map.tryFind "green" m |> Option.defaultValue 0;
        Blue = Map.tryFind "blue" m |> Option.defaultValue 0 }

let parseCombos (s:string) =
    Seq.map parseCombo (s.Split("; "))
        |> Seq.toList

let parseGame s =
    let m = Regex.Match(s, "^Game ([0-9]+): (.*)$")
    if m.Success then
        let id = int m.Groups.[1].Value
        let combos = parseCombos m.Groups.[2].Value
        { ID = id; Combos = combos }
    else
        failwithf "Cannot parse game from '%s'" s

let possibleGame red green blue game =
    not (List.exists (fun combo -> combo.Red > red) game.Combos)
    && not (List.exists (fun combo -> combo.Green > green) game.Combos)
    && not (List.exists (fun combo -> combo.Blue > blue) game.Combos)

System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
    |> Seq.map parseGame
    |> Seq.filter (possibleGame 12 13 14)
    |> Seq.map (fun game -> game.ID)
    |> Seq.sum
    |> printfn "%A"
