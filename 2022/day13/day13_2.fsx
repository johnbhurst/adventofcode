#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-28
// See https://adventofcode.com/2022/day/13

open System.Text.RegularExpressions

type Token =
    | Number of int
    | LeftBracket
    | RightBracket
    | Comma

let rec pair items =
    match items with
    | [] -> []
    | item1 :: item2 :: rest -> (item1, item2) :: pair rest
    | _ -> failwith "odd number of items"

let digits (s:string) =
    let m = Regex.Match(s, "([0-9]+)(.*)")
    if m.Success then
        int m.Groups.[1].Value, m.Groups.[2].Value
    else
        failwithf "no digits in '%s'" s

let rec tokenise (line:string) =
    match line with
    | "" -> []
    | _ -> match line.[0] with
            | '[' -> LeftBracket :: tokenise line.[1..]
            | ']' -> RightBracket :: tokenise line.[1..]
            | ',' -> Comma :: tokenise line.[1..]
            | _ ->
                let number, rest = digits line
                Number (int number) :: tokenise rest

let rec parseList (tokens:Token list) : obj list * Token list =
    match tokens with
    | LeftBracket :: rest ->
        let items, rest1 = parseItems rest []
        items, rest1
    | _ -> failwithf "unexpected token, expected '[', got %A" (List.head tokens)
and parseItems (tokens:Token list) (items: obj list) : obj list * Token list =
    match tokens with
    | LeftBracket :: rest ->
        let item, rest1 = parseList tokens
        parseAfterItem rest1 (items @ [item])
    | Number item :: rest ->
        parseAfterItem rest (items @ [item])
    | RightBracket :: rest -> [], rest
    | _ -> failwithf "unexpected token, expected item, got %A" (List.head tokens)
and parseAfterItem (tokens:Token list) (items: obj list) : obj list * Token list =
    match tokens with
    | RightBracket :: rest -> items, rest
    | Comma :: rest -> parseItems rest items
    | _ -> failwithf "unexpected token, expected ']' or ',', got %A" (List.head tokens)

type Order =
    | RightOrder
    | WrongOrder
    | Unknown

let rec correctOrder (items1:obj list) (items2:obj list) =
    match items1, items2 with
    | [], [] -> Unknown
    | [], _ -> RightOrder
    | _, [] -> WrongOrder
    | head1 :: rest1, head2 :: rest2 ->
        match head1, head2 with
        | (:? int as v1), (:? int as v2) -> if v1 < v2 then RightOrder
                                            elif v1 > v2 then WrongOrder
                                            else correctOrder rest1 rest2
        | (:? int as v1), (:? (obj list) as v2) ->
            match correctOrder [v1] v2 with
            | RightOrder -> RightOrder
            | WrongOrder -> WrongOrder
            | Unknown -> correctOrder rest1 rest2
        | (:? (obj list) as v1), (:? int as v2) ->
        match correctOrder v1 [v2] with
            | RightOrder -> RightOrder
            | WrongOrder -> WrongOrder
            | Unknown -> correctOrder rest1 rest2
        | (:? (obj list) as v1), (:? (obj list) as v2) ->
            match correctOrder v1 v2 with
            | RightOrder -> RightOrder
            | WrongOrder -> WrongOrder
            | Unknown -> correctOrder rest1 rest2
        | _ -> failwithf "unexpected %A %A" head1 head2

let divider1 = "[[2]]" |> tokenise |> parseList |> fst
let divider2 = "[[6]]" |> tokenise |> parseList |> fst

System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
    |> Seq.toList
    |> List.filter (fun line -> line <> "")
    |> List.map tokenise
    |> List.map (fun tokens -> parseList tokens |> fst)
    |> (fun l -> divider1 :: divider2 :: l)                   // add dividers
    |> List.sortWith (fun item1 item2 ->
                        let order = correctOrder item1 item2
                        match order with
                        | RightOrder -> -1
                        | WrongOrder -> 1
                        | Unknown -> 0
                        )                                      // all items in order, including dividers
    |> List.mapi (fun i item -> (i+1, item))                   // collect with index
    |> List.filter (fun (i, item) -> item = divider1 || item = divider2) // filter out dividers
    |> List.map fst                                            // extract indices
    |> List.reduce (*)                                         // multiply indices of dividers for result
    |> printfn "%d"
