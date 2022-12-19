#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-04

open System.Text.RegularExpressions

type StackSet = { n: int; items: char list[] }

// split a string into a list of 3-character strings
let rec splitTo3 str =
    match str with
    | "" -> []
    | _ -> str.[0..2] :: splitTo3 str.[4..]

// split a list of strings into a pair of lists, where the first list ends with an empty string
let rec split input =
    match input with
    | [] -> ([], [])
    | line :: rest ->
        if line = "" then
            ([], rest)
        else
            let (first, rest) = split rest
            (line :: first, rest)

// create a StackSet of size n
let create n =
    { n = n; items = Array.init n (fun _ -> []) }

// push item to ith stack of StackSet
let pushItem i item stackSet =
    stackSet.items.[i] <- item :: stackSet.items.[i]
    stackSet

// pop an item from ith stack of StackSet
let popItem i stackSet =
    match stackSet.items.[i] with
    | [] -> failwith "popItem: empty stack"
    | item :: rest ->
        stackSet.items.[i] <- rest
        item

// move n items from src stack to dst stack in StackSet
let move n src dst stackSet =
    for i in 1..n do
        pushItem dst (popItem src stackSet) stackSet |> ignore
    stackSet

// recursive version. It's debatable whether this is more readable than the loop version.
// let rec move n src dst stackSet =
//     match n with
//     | 0 -> stackSet
//     | _ -> move (n - 1) src dst (pushItem dst (popItem src stackSet) stackSet)

// parse a create line to return a StackSet
let parseCreate str =
    let createRegex = Regex(@"^( +\d)+$")
    let m = createRegex.Match(str)
    if m.Success then
        let n = int m.Groups.[1].Value
        create n
    else
        failwith "parseCreate: invalid create"

// parse a setup line to return a list of operations
let parseSetup str =
    let setupRegex = Regex(@"^\[([A-Z])\]$")
    let parse1 i x =
        let m = setupRegex.Match(x)
        if m.Success then
            Some(pushItem i m.Groups.[1].Value.[0])
        elif x = "   " then
            None
        else
            failwith "parse1Setup: invalid setup"
    splitTo3 str |> List.mapi parse1

// parse a move line to return an operation
let parseMove str =
    let moveRegex = Regex(@"^move (\d+) from (\d+) to (\d+)$")
    let m = moveRegex.Match(str)
    if m.Success then
        let n = int m.Groups.[1].Value
        let src = int m.Groups.[2].Value
        let dst = int m.Groups.[3].Value
        move n (src-1) (dst-1)
    else
        failwith "parseMove: invalid move"

// apply a list of operations to an object
let applyTo obj ops =
    ops |> List.fold (fun acc op -> op acc) obj

// MAIN PROGRAM
let lines = System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
                |> List.ofSeq

let (stackLines, moveLines) = split lines
let (createLine, setupLines) =
    match (List.rev stackLines) with
    | [] -> failwith "no create line"
    | head :: rest -> (head, rest)

// printfn "stackLines=[%A]" stackLines
// printfn "moveLines=[%A]" moveLines
// printfn "createLine=[%A]" createLine
// printfn "setupLines=[%A]" setupLines

let initialStacks = parseCreate createLine
let setups = setupLines |> List.map parseSetup |> List.concat |> List.choose id
let moves = moveLines |> List.map parseMove

let stacks = setups |> applyTo initialStacks
let finalStacks = moves |> applyTo stacks
// printfn "stacks=[%A]" stacks
// printfn "finalStacks=[%A]" finalStacks

finalStacks.items
    |> Array.map List.head
    |> Array.map string
    |> String.concat ""
    |> printfn "%s"
