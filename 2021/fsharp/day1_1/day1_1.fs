// Try advent 2021 day 1 in F#

let rec countLarger l =
    match l with
    | h1::h2::t -> (if h2 > h1 then 1 else 0) + countLarger (h2::t)
    | _ -> 0

let input = System.IO.File.ReadAllLines( System.Environment.GetCommandLineArgs().[1] ) |> Array.map int |> List.ofArray
let result = countLarger input
printfn "%d" result
