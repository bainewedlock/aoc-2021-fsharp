open Solution

let input = System.IO.File.ReadAllText "input.txt"

[<EntryPoint>]
let main argv =

    //solve input
    //|> printfn "solution 1: %A"
    printfn "(accepted answer: 7626)"
    // solved with excel:
    // step 1: find the maximum x velocity which stabilizes inside the area
    // step 2: find maximum y velocity between 1-200 which hits in the area

    //solve2 input
    //|> printfn "solution 2: %A"
    //printfn "(accepted answer: ?)"

    0
