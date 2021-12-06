module Solution

let parse (input:string) =
    input.Split ','
    |> Array.toList
    |> List.map int
    |> List.groupBy id
    |> List.map (fun (k,v) -> k, uint64 v.Length)

let step fs =
    fs |> List.collect (fun (x,n) ->
        if x = 0
        then [6, n; 8, n]
        else [x-1, n])

let reduce =
    List.groupBy fst
    >> List.map (fun (k,vs) -> k, List.sumBy snd vs)

let genericSolve totalDays (input:string) =
    let rec loop days fs =
        if days = 0 then fs else
        loop (days-1) (fs |> step |> reduce)
    loop totalDays (parse input)
    |> List.sumBy snd
    |> sprintf "%d"

let solve  = genericSolve 80
let solve2 = genericSolve 256
