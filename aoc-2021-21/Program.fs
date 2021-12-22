open Solution

let input = System.IO.File.ReadAllText "input.txt"

[<EntryPoint>]
let main argv =

    solve 1 2
    |> printfn "solution 1: %A"
    printfn "(accepted answer: 598416)"

    solve2 input
    |> printfn "solution 2: %A"
    printfn "(accepted answer: ?)"

    0
