module Solution

let eval fFuel ns target =
    List.sumBy ((-)target >> abs >> fFuel) ns

let genericSolve fFuel (input:string) =
    let ns = input.Split "," |> Seq.map int |> Seq.toList
    let min = List.min ns
    let max = List.max ns
    [min..max]
    |> List.map (eval fFuel ns)
    |> List.min

let sumOfRange a = a * (a+1) / 2

let solve = genericSolve id
let solve2 = genericSolve sumOfRange
