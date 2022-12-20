#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-11
// See https://adventofcode.com/2022/day/7

// lines match one of these regexes:
// @"^\$ cd /$"
// @"^\$ cd [a-z]+$"
// @"^\$ cd ..$"
// @"^\$ ls$"
// @"^[0-9]+ [a-z]+$"
// @"^[0-9]+ [a-z]+\\.[a-z]+$"
// @"^dir [a-z]+"$"

type File = { Size: int; Name: string }
type Dir = { Name: string; Files: File list; Dirs: Dir list }

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

let rec parseDir lines =
    match lines with
    | CdDir dirname :: rest -> parseLs dirname rest
    | _ -> failwith "Expected 'cd dir'"
and parseLs dirname lines =
    match lines with
    | Ls :: rest -> parseEntries rest dirname [] []
    | _ -> failwith "Expected 'ls'"
and parseEntries lines dirname files dirs =
    match lines with
    | CdUp :: rest -> { Name = dirname; Files = files; Dirs = dirs}, rest
    | Dir _ :: rest -> parseEntries rest dirname files dirs
    | File (size, filename) :: rest -> parseEntries rest dirname ({ Size = size; Name = filename } :: files) dirs
    | CdDir dir :: rest ->
        let result, rest1 = parseDir lines
        parseEntries rest1 dirname files (result :: dirs)
    | [] -> { Name = dirname; Files = files; Dirs = dirs}, []
    | _ -> failwithf "Expected 'cd' or file/directory entry"

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

let sizes = dirSizes dir
let totalSize = 70000000
let usedSize = List.head sizes
let freeSize = totalSize - usedSize
let updateSize = 30000000
let requiredSize = updateSize - freeSize

sizes
    |> List.filter (fun size -> size > requiredSize)
    |> List.min
    |> printfn "%d"
