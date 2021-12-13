open Solution

let input = System.IO.File.ReadAllText "input.txt"

[<EntryPoint>]
let main argv =

    solve input
    |> printfn "solution 1: %A"
    printfn "(accepted answer: 765)"

    solve2 input
    |> printfn "solution 2: %A"
    printfn "(accepted answer: RZKZLPGH)"

    0
