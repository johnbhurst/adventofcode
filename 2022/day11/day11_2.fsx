#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-22
// See https://adventofcode.com/2022/day/11
// See https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/active-patterns

// In this implementation, the worry levels grow ridiculously large, and cannot be computed to full precision in a reasonable time.
// Instead, we record the worry level as a set of remainders modulo the test divisors. This will be sufficient for evaluating the test condition for each monkey.

open System.Text.RegularExpressions

type Monkey = { Monkey: int; StartingItems: int list; Operation: int -> int -> int; TestDivisible: int; IfTrue: int; IfFalse: int }

let (|Add|_|) (str:string) = if str.StartsWith("new = old + ") then Some(str.Substring(12) |> int) else None
let (|Multiply|_|) (str:string) = if str.StartsWith("new = old * ") then Some(str.Substring(12) |> int) else None
let (|Square|_|) (str:string) = if str = "new = old * old" then Some() else None

let matchRegex regexStr str =
    let m = Regex(regexStr).Match(str)
    if m.Success then m.Groups.[1].Value else failwith ("Expected " + regexStr + ", got line " + str)

let rec parse lines =
    match (lines: string list) with
    | monkeyLine :: startingItemsLine :: operationLine :: testLine :: ifTrueLine :: ifFalseLine :: rest ->

        let monkey = matchRegex "Monkey ([0-9]+):" monkeyLine |> int
        let startingItems = matchRegex "  Starting items: ([0-9]+(, [0-9]+)*)" startingItemsLine |> (fun s -> s.Split(",")) |> Seq.map int |> List.ofSeq
        let operation = match matchRegex "  Operation: (.*)" operationLine with
                        | Square _ -> fun divisor worry -> (worry * worry) % divisor
                        | Multiply n -> fun divisor worry -> (worry * n) % divisor
                        | Add n -> fun divisor worry -> (worry + n) % divisor
                        | _ -> failwith ("Unknown operation" + operationLine)
        let testDivisible = matchRegex "  Test: divisible by ([0-9]+)" testLine |> int
        let ifTrue = matchRegex "    If true: throw to monkey ([0-9]+)" ifTrueLine |> int
        let ifFalse = matchRegex "    If false: throw to monkey ([0-9]+)" ifFalseLine |> int
        { Monkey = monkey; StartingItems = startingItems; Operation = operation; TestDivisible = testDivisible; IfTrue = ifTrue; IfFalse = ifFalse } :: parse rest
    | [] -> []
    | _ -> failwith "Unexpected number of lines"

let monkeys = System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
                |> Seq.toList
                |> List.filter (fun line -> line <> "")
                |> parse

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
    remainderMap.Keys |> Seq.map (fun divisor -> divisor, operation divisor remainderMap.[divisor]) |> Map

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

inspections
    |> Array.sortDescending
    |> Array.take 2
    |> Array.map bigint
    |> Array.reduce (fun x y -> x * y)
    |> printfn "%A"
