#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-22
// See https://adventofcode.com/2022/day/11
// See https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/active-patterns

type Monkey = { Monkey: int; mutable Items: int list; Operation: string; TestDivisible: int; IfTrue: int; IfFalse: int }

let (|Add|_|) (str:string) = if str.StartsWith("new = old + ") then Some(str.Substring(12) |> int) else None
let (|Multiply|_|) (str:string) = if str.StartsWith("new = old * ") then Some(str.Substring(12) |> int) else None
let (|Square|_|) (str:string) = if str = "new = old * old" then Some() else None

let rec parseMonkeys (lines: string list) =
    match lines with
    | [] -> []
    | line :: rest ->
        if line.StartsWith("Monkey ") then
            let s = line.Substring(7)
            let monkey = s.[0..s.Length - 2] |> int
            parseStartingItems rest monkey
        else
            failwith ("Expected 'Monkey: ', got line " + line)
and parseStartingItems lines monkey =
    match lines with
    | line :: rest ->
        if line.StartsWith("  Starting items: ") then
            let startingItems = line.Substring(18).Split(", ") |> Seq.map int |> List.ofSeq
            parseOperation rest monkey startingItems
        else
            failwith ("Expected 'Starting items: ', got line " + line)
    | _ -> failwith "Expected 'Starting items: '"
and parseOperation lines monkey startingItems =
    match lines with
    | line :: rest ->
        if line.StartsWith("  Operation: ") then
            let operation = line.Substring(13)
            parseTestDivisible rest monkey startingItems operation
        else
            failwith ("Expected 'Operation: ', got line " + line)
    | _ -> failwith "Expected 'Operation: '"
and parseTestDivisible lines monkey startingItems operation =
    match lines with
    | line :: rest ->
        if line.StartsWith("  Test: divisible by ") then
            let testDivisible = line.Substring(21) |> int
            parseIfTrue rest monkey startingItems operation testDivisible
        else
            failwith ("Expected 'Test divisible: ', got line " + line)
    | _ -> failwith "Expected 'Test divisible: '"
and parseIfTrue lines monkey startingItems operation testDivisible =
    match lines with
    | line :: rest ->
        if line.StartsWith("    If true: throw to monkey ") then
            let ifTrue = line.Substring(29) |> int
            parseIfFalse rest monkey startingItems operation testDivisible ifTrue
        else
            failwith ("Expected 'If true: ', got line " + line)
    | _ -> failwith "Expected 'If true: '"
and parseIfFalse lines monkey startingItems operation testDivisible ifTrue =
    match lines with
    | line :: rest ->
        if line.StartsWith("    If false: throw to monkey ") then
            let ifFalse = line.Substring(30) |> int
            { Monkey = monkey; Items = startingItems; Operation = operation; TestDivisible = testDivisible; IfTrue = ifTrue; IfFalse = ifFalse } :: parseMonkeys rest
        else
            failwith ("Expected 'If false: ', got line " + line)
    | _ -> failwith "Expected 'If false: '"

let monkeys = System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
                |> Seq.toList
                |> List.filter (fun x -> x <> "")
                |> parseMonkeys

let monkeyCount = List.length monkeys

let inspections = Array.zeroCreate<int> monkeyCount

let doItem (monkey:Monkey) (worry:int) =
    inspections.[monkey.Monkey] <- inspections.[monkey.Monkey] + 1
    let increasedWorry = match monkey.Operation with
                            | Square _ -> worry * worry
                            | Multiply n -> worry * n
                            | Add n -> worry + n
                            | _ -> failwith "Unknown operation"
    let newWorry = increasedWorry / 3
    let target = if newWorry % monkey.TestDivisible = 0 then monkey.IfTrue else monkey.IfFalse
    monkeys.[target].Items <- monkeys.[target].Items @ [newWorry]

let doMonkey (monkey:Monkey) =
    monkey.Items |> List.iter (doItem monkey)
    monkey.Items <- []

for round in 1..20 do
    monkeys |> List.iter doMonkey

let topInspections = inspections |> Array.sortDescending |> Array.take 2
printfn "%d" (topInspections.[0] * topInspections.[1])
