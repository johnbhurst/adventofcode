#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-22
// See https://adventofcode.com/2022/day/11
// See https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/active-patterns

open System.Text.RegularExpressions

type Monkey = { Monkey: int; mutable Items: int list; Operation: int -> int; TestDivisible: int; IfTrue: int; IfFalse: int }

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
                        | Square _ -> fun worry -> worry * worry
                        | Multiply n -> fun worry -> worry * n
                        | Add n -> fun worry -> worry + n
                        | _ -> failwith ("Unknown operation" + operationLine)
        let testDivisible = matchRegex "  Test: divisible by ([0-9]+)" testLine |> int
        let ifTrue = matchRegex "    If true: throw to monkey ([0-9]+)" ifTrueLine |> int
        let ifFalse = matchRegex "    If false: throw to monkey ([0-9]+)" ifFalseLine |> int
        { Monkey = monkey; Items = startingItems; Operation = operation; TestDivisible = testDivisible; IfTrue = ifTrue; IfFalse = ifFalse } :: parse rest
    | [] -> []
    | _ -> failwith "Unexpected number of lines"

let monkeys = System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
                |> Seq.toList
                |> List.filter (fun line -> line <> "")
                |> parse

let monkeyCount = List.length monkeys

let inspections = Array.zeroCreate<int> monkeyCount

let doMonkey (monkey:Monkey) =
    monkey.Items |> List.iter (fun worry ->
        inspections.[monkey.Monkey] <- inspections.[monkey.Monkey] + 1
        let increasedWorry = monkey.Operation worry
        let newWorry = increasedWorry / 3
        let target = if newWorry % monkey.TestDivisible = 0 then monkey.IfTrue else monkey.IfFalse
        monkeys.[target].Items <- monkeys.[target].Items @ [newWorry]
    )
    monkey.Items <- []

for round in 1..20 do
    monkeys |> List.iter doMonkey

inspections
    |> Array.sortDescending
    |> Array.take 2
    |> Array.reduce (*)
    |> printfn "%d"
