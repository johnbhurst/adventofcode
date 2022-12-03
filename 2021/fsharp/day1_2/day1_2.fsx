#!/usr/bin/env -S dotnet fsi
// Try advent 2021 day 1 in F#

let rec countLargerWindow l =
    match l with
    | h1::h2::h3::h4::t -> (if h2+h3+h4 > h1+h2+h3 then 1 else 0) + countLargerWindow (h2::h3::h4::t)
    | _ -> 0

let result = System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
                |> List.ofSeq
                |> List.map int
                |> countLargerWindow
printfn "%d" result
