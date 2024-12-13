#!/usr/bin/env -S dotnet fsi
// Copyright 2024 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2024-12-06
// See https://adventofcode.com/2024/day/5

let input =
    if fsi.CommandLineArgs.Length > 1
        then System.IO.File.ReadLines(fsi.CommandLineArgs.[1])
        else ("""47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47""".Split('\n'))

// split list of lines into list of rules and list of updates, with a blank line separating them
let rec splitLines lines =
    match lines with
    | "" :: rest -> ([], rest)
    | line :: rest -> let (rules, updates) = splitLines rest
                      (line :: rules, updates)
    | [] -> ([], [])

let ruleLines, updateLines =
    input
    |> Seq.toList
    |> splitLines

let parseRule (line:string) =
    let parts = line.Split('|')
    let part1 = int(parts.[0])
    let part2 = int(parts.[1])
    (part1, part2)

let parseUpdate (line:string) =
    line.Split(',') |> List.ofArray |> List.map int

let rules = ruleLines |> List.map parseRule
// [(47, 53); (97, 13); (97, 61); (97, 47); (75, 29); (61, 13); (75, 53); (29, 13);
//  (97, 29); (53, 29); (61, 53); (97, 53); (61, 29); (47, 13); (75, 47); (97, 75);
//  (47, 61); (75, 61); (47, 29); (75, 13); (53, 13)]

let updates = updateLines |> List.map parseUpdate
// [[75; 47; 61; 53; 29]; [97; 61; 53; 29; 13]; [75; 29; 13]; [75; 97; 47; 61; 53];
//  [61; 13; 29]; [97; 13; 75; 29; 47]]

let rec ruleOk page rest (page1, page2) =
    if page <> page2 then true // rule doesn't apply
    else not (List.contains page1 rest)

let updateOk updates =
    match updates with
    | page :: rest -> List.forall (ruleOk page rest) rules
    | [] -> raise (System.ArgumentException("Empty update"))

let rec updatesOk updates =
    match updates with
    | page :: rest -> (updateOk (page :: rest)) && (updatesOk rest)
    | [] -> true

let middleElement list =
    let len = List.length list
    let mid = len / 2
    match List.tryItem mid list with
    | Some x -> x
    | None -> raise (System.ArgumentException("List is empty"))

updates
    |> List.filter updatesOk
    |> List.map middleElement
    |> List.sum
    |> printfn "%A"
