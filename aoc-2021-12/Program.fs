open Solution

let input = System.IO.File.ReadAllText "input.txt"

[<EntryPoint>]
let main argv =

    solve input
    |> printfn "solutio 1: %A"
    printfn "(accepted answer: 3887)"

    solve2 input
    |> printfn "solution 2: %A"
    printfn "(accepted answer: 104834)"

    0
