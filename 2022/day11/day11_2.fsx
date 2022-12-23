#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-22
// See https://adventofcode.com/2022/day/11
// See https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/active-patterns

// In this implementation, the worry levels grow ridiculously large, and cannot be computed to full precision in a reasonable time.
// Instead, we record the worry level as a set of remainders modulo the test divisors. This will be sufficient for evaluating the test condition for each monkey.

type Monkey = { Monkey: int; StartingItems: int list; Operation: int -> int -> int; TestDivisible: int; IfTrue: int; IfFalse: int }

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
            let operation = match (line.Substring(13)) with
                            | Square _ -> fun divisor worry -> (worry * worry) % divisor
                            | Multiply n -> fun divisor worry -> (worry * n) % divisor
                            | Add n -> fun divisor worry -> (worry + n) % divisor
                            | _ -> failwith "Unknown operation"
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
            { Monkey = monkey; StartingItems = startingItems; Operation = operation; TestDivisible = testDivisible; IfTrue = ifTrue; IfFalse = ifFalse } :: parseMonkeys rest
        else
            failwith ("Expected 'If false: ', got line " + line)
    | _ -> failwith "Expected 'If false: '"

let monkeys = System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
                |> Seq.toList
                |> List.filter (fun x -> x <> "")
                |> parseMonkeys

// for a given starting item, make the map of all the monkey divisors to the remainder of the starting item modulo that divisor
let makeRemainderMap worry =
    monkeys
    |> List.map (fun monkey -> monkey.TestDivisible)
    |> List.map (fun divisor -> divisor, worry % divisor)
    |> Map

// for a given monkey, make the list of maps of all the monkey divisors to the remainder of each of the monkey's starting items
let makeRemainderMaps monkey =
    monkey.StartingItems |> List.map makeRemainderMap

// (mutable) array of each monkey's list of items, with each item being a map of all the monkey divisors and item's remainder modulo that divisor
let monkeyRemainderMaps =
    monkeys |> List.map makeRemainderMaps |> Array.ofList

// (mutable) array of the number of items inspected by each monkey
let inspections = Array.zeroCreate<int> (List.length monkeys)

// apply the operation to an item's remainder map to get an updated remainder map
let applyOperation (operation: int -> int -> int) (remainderMap:Map<int, int>) =
    remainderMap.Keys |> Seq.map (fun x -> x, operation x remainderMap.[x]) |> Map

// process an item on a monkey
let doItem (monkey:Monkey) (remainderMap:Map<int, int>) =
    inspections.[monkey.Monkey] <- inspections.[monkey.Monkey] + 1
    let updatedRemainderMap = applyOperation monkey.Operation remainderMap
    let target = if updatedRemainderMap.[monkey.TestDivisible] = 0 then monkey.IfTrue else monkey.IfFalse
    monkeyRemainderMaps.[target] <- monkeyRemainderMaps.[target] @ [updatedRemainderMap]

// process all the items for a monkey
let doMonkey (monkey:Monkey) =
    monkeyRemainderMaps.[monkey.Monkey] |> List.iter (doItem monkey)
    monkeyRemainderMaps.[monkey.Monkey] <- []

for round in 1..10000 do
    monkeys |> List.iter doMonkey

let topInspections = inspections |> Array.sortDescending |> Array.take 2 |> Array.map bigint
printfn "%A" (topInspections.[0] * topInspections.[1])
