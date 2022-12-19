#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-11

// lines match one of these regexes:
// @"^\$ cd /$"
// @"^\$ cd [a-z]+$"
// @"^\$ cd ..$"
// @"^\$ ls$"
// @"^[0-9]+ [a-z]+$"
// @"^[0-9]+ [a-z]+\\.[a-z]+$"
// @"^dir [a-z]+"$"

let (|CdUp|_|) str = if str = "$ cd .." then Some() else None

let (|CdDir|_|) (str:string) = if str.StartsWith("$ cd ") then Some(str.Substring(5)) else None

let (|Ls|_|) str = if str = "$ ls" then Some() else None

let (|Dir|_|) (str:string) = if str.StartsWith("dir ") then Some(str.Substring(4)) else None

let (|File|_|) (str:string) =
    let parts = str.Split(' ')
    if parts.Length = 2 then
        let size = int parts.[0]
        let name = parts.[1]
        Some(size, name)
    else
        None

type File = { Size: int; Name: string }
type Dir = { Name: string; Files: File list; Dirs: Dir list }

let rec parseDir lines =
    match lines with
    | [] -> failwith "Expected 'cd dir', got end of input"
    | line :: rest ->
        match line with
        | CdDir dirname -> parseLs dirname rest
        | _ -> failwithf "Expected 'cd dir', got line: %s" line
and parseLs dirname lines =
    match lines with
    | [] -> failwith "Expected 'ls', got end of input"
    | line :: rest ->
        match line with
        | Ls -> parseEntries rest dirname [] []
        | _ -> failwithf "Expecting 'ls', got line: %s" line
and parseEntries lines dirname files dirs =
    match lines with
    | [] -> { Name = dirname; Files = files; Dirs = dirs}, []
    | line :: rest ->
        match line with
        | CdUp -> { Name = dirname; Files = files; Dirs = dirs}, rest
        | Dir _ -> parseEntries rest dirname files dirs
        | File (size, filename) -> parseEntries rest dirname ({ Size = size; Name = filename } :: files) dirs
        | CdDir dir ->
            let result, rest1 = parseDir lines
            parseEntries rest1 dirname files (result :: dirs)
        | _ -> failwithf "Expected 'cd' or file/directory entry, got line: %s" line

let rec dirSize { Name = _; Files = files; Dirs = dirs } =
    let filesSize = files |> List.map (fun f -> f.Size) |> List.sum
    let dirsSize = dirs |> List.map dirSize |> List.sum
    filesSize + dirsSize

let rec dirSizes dir =
    dirSize dir :: (List.map dirSizes dir.Dirs |> List.concat)

let dir, rest = System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
                    |> List.ofSeq
                    |> parseDir

match rest with
| [] -> ()
| _ -> failwith "Expected end of input, got more lines"

dir
    |> dirSizes
    |> List.filter (fun x -> x <= 100000)
    |> List.sum
    |> printfn "%d"
