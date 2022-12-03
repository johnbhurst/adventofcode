// Try advent 2021 day 1 in F#

let rec countLarger l =
    match l with
    | h1::h2::t -> (if h2 > h1 then 1 else 0) + countLarger (h2::t)
    | _ -> 0

let result = System.IO.File.ReadLines( fsi.CommandLineArgs.[1] )
                |> List.ofSeq
                |> List.map int
                |> countLarger
printfn "%d" result
