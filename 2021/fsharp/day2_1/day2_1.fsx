// Copyright 2022 John Hurst
// John Hurst (john.b.hurst@gmail.com)
// 2022-12-01

type Command =
    | Forward of int
    | Up of int
    | Down of int
type Position = { horizontal: int; vertical: int }

let update position command =
    match command with
    | Forward n -> { position with horizontal = position.horizontal + n }
    | Up n -> { position with vertical = position.vertical - n }
    | Down n -> { position with vertical = position.vertical + n }

let score position = position.horizontal * position.vertical

let parse (line: string) =
    let items = line.Split ' '
    let command = items.[0]
    let n = int items.[1]
    match command with
    | "forward" -> Forward n
    | "up" -> Up n
    | "down" -> Down n
    | _ -> failwith "Invalid command"

let result = System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
                |> List.ofSeq
                |> List.map parse
                |> List.fold update { horizontal = 0; vertical = 0 }
                |> score
printfn "%d" result
