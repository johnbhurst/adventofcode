#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-04

open System.Text.RegularExpressions

let rec splitTo3 str =
    match str with
    | "" -> []
    | _ -> str.[0..2] :: splitTo3 str.[4..]

let rec split input =
    match input with
    | [] -> ([], [])
    | line :: rest ->
        if line = "" then
            ([], rest)
        else
            let (first, rest) = split rest
            (line :: first, rest)

type StackSet = { n: int; items: char list[] }

// function to create a StackSet of size n
let create n =
    { n = n; items = Array.init n (fun _ -> []) }

// function to push item to StackSet
let pushItem x item stackSet =
    stackSet.items.[x] <- item :: stackSet.items.[x]
    stackSet

// function to pop an item from StackSet
let popItem x stackSet =
    match stackSet.items.[x] with
    | [] -> failwith "popItem: empty stack"
    | item :: rest ->
        stackSet.items.[x] <- rest
        item

// let rec move n src dst stackSet =
//     match n with
//     | 0 -> stackSet
//     | _ -> move (n - 1) src dst (pushItem dst (popItem src stackSet) stackSet)

let move n src dst stackSet =
    for i in 1..n do
        pushItem dst (popItem src stackSet) stackSet |> ignore
    stackSet

let createCommand str =
    create 3

let lines = System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
                |> List.ofSeq

let (stackLines, moveLines) = split lines
let (createLine, setupLines) =
    match (List.rev stackLines) with
    | [] -> failwith "no create line"
    | h :: t -> (h, t)

// printfn "stackLines=[%A]" stackLines
// printfn "moveLines=[%A]" moveLines
// printfn "createLine=[%A]" createLine
// printfn "setupLines=[%A]" setupLines

let moveRegex = Regex(@"^move (\d+) from (\d+) to (\d+)$")

let parseMove str =
    let m = moveRegex.Match(str)
    if m.Success then
        let n = int m.Groups.[1].Value
        let src = int m.Groups.[2].Value
        let dst = int m.Groups.[3].Value
        move n (src-1) (dst-1)
    else
        failwith "parseMove: invalid move"

let createRegex = Regex(@"^( +\d)+$")

let parseCreate str =
    let m = createRegex.Match(str)
    if m.Success then
        let n = int m.Groups.[1].Value
        create n
    else
        failwith "parseCreate: invalid create"

let setupRegex = Regex(@"^\[([A-Z])\]$")

let (|Setup|_|) str =
    let m = setupRegex.Match(str)
    if m.Success then Some(m.Groups.[1].Value.[0])
    else None

let (|Nop|_|) str =
    match str with
    | "   " -> Some()
    | _ -> None

let parse1Setup i x =
    match x with
    | Setup x -> Some(pushItem i x)
    | Nop _ -> None
    | _ -> failwith "parse1Setup: invalid setup"

let parseSetup str =
    splitTo3 str |> List.mapi parse1Setup

let initialStacks = parseCreate createLine
let setups = setupLines |> List.map parseSetup |> List.concat |> List.choose id

let stacks = List.fold (fun s setup -> setup s) initialStacks setups
// printfn "stacks=[%A]" stacks

let moves = moveLines |> List.map parseMove
let finalStacks = moves |> List.fold (fun s f -> f s) stacks

// printfn "finalStacks=[%A]" finalStacks

let result = finalStacks.items |> Array.map List.head |> Array.map string |> String.concat ""
printfn "%s" result