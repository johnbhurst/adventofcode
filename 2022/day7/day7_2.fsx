#!/usr/bin/env -S dotnet fsi
// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-11
// See https://adventofcode.com/2022/day/7

// lines match one of these patterns:
// $ cd /
// $ cd [a-z]+
// $ cd ..
// $ ls
// [0-9]+ [a-z]+
// [0-9]+ [a-z]+\\.[a-z]+
// dir [a-z]+

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
        Some( {Size = size; Name = name} )
    else
        None

let rec parseDir lines =
    match lines with
    | CdDir name :: rest -> parseLs rest name
    | _ -> failwith "Expected 'cd dir'"
and parseLs lines name  =
    match lines with
    | Ls :: rest -> parseEntries rest name [] []
    | _ -> failwith "Expected 'ls'"
and parseEntries lines name files dirs =
    match lines with
    | CdUp :: rest -> { Name = name; Files = files; Dirs = dirs}, rest
    | Dir _ :: rest -> parseEntries rest name files dirs
    | File file :: rest ->
        parseEntries rest name (file :: files) dirs
    | CdDir _ :: _ ->
        let dir, rest' = parseDir lines
        parseEntries rest' name files (dir :: dirs)
    | [] -> { Name = name; Files = files; Dirs = dirs}, []
    | _ -> failwithf "Expected 'cd' or file/directory entry"

// total size of this directory and its children
let rec dirSize { Name = _; Files = files; Dirs = dirs } =
    let filesSize = files |> List.map (fun f -> f.Size) |> List.sum
    let dirsSize = dirs |> List.map dirSize |> List.sum
    filesSize + dirsSize

// list of total sizes of this directory and of each of its children
let rec dirSizes dir =
    dirSize dir :: (List.map dirSizes dir.Dirs |> List.concat)

// main program
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
let sufficientSize size = size > requiredSize

sizes
    |> List.filter sufficientSize
    |> List.min
    |> printfn "%d"
