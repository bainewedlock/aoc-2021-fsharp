module Solution

open System

// group characters at string index i
// find the min/max group based on: (group length, character)
// (the snd part = character '1'/'0' is relevant for part 2)
let analyze f i (xs:string seq) =
    xs
    |> Seq.groupBy (fun x -> x.[i])
    |> f (fun (k,xs) -> (Seq.length xs, k))

let keepAt f i = analyze f i >> snd >> Seq.toList

let extract f i = analyze f i >> fst

let parse (input:string) =
    input.Split "\r\n"

let extractAll f (xs:string[]) =
    let n = xs.[0].Length
    [|0..n-1|]
    |> Array.map (fun i -> extract f i xs)
    |> String

let bin2dec (s:string) = Convert.ToInt32(s, 2)
    
let find f (xs:string[]) =
    let rec loop i = function
    | [x] -> x
    | xs2 -> keepAt f i xs2 |> loop (i+1)
    loop 0 (Array.toList xs)

let genericSolve f input =
    let xs = parse input
    let a = f Seq.maxBy xs
    let b = f Seq.minBy xs
    bin2dec a * bin2dec b

let solve  = genericSolve extractAll
let solve2 = genericSolve find
