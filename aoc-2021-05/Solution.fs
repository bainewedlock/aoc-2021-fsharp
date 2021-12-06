module Solution

open System.Text.RegularExpressions
open System

let dist a b = abs (a-b)

let vertical   x y1 y2 = Set [ for y in y1..y2 do yield (x,y) ]
let horizontal y x1 x2 = Set [ for x in x1..x2 do yield (x,y) ]
let diagonal x1 y1 x2 y2 = Set [
    let dx = x2-x1 |> int |> Math.Sign
    let dy = y2-y1 |> int |> Math.Sign
    for i in 0..(dist x1 x2) do
        yield (x1+i*dx,y1+i*dy) ]

let isDiagonal (x1,y1,x2,y2) = dist x1 x2 = dist y1 y2 

let pixels (x1,y1,x2,y2) =
    if x1=x2 then vertical   x1 (min y1 y2) (max y1 y2) else
    if y1=y2 then horizontal y1 (min x1 x2) (max x1 x2) else
    if isDiagonal (x1,y1,x2,y2) then diagonal x1 y1 x2 y2 else
    Set.empty

let parseLine (line:string) =
    let ns =
        Regex.Matches(line, @"-?\d+")
        |> Seq.map (fun x -> int x.Value)
        |> Seq.toList
    ns.[0], ns.[1], ns.[2], ns.[3]

let genericSolve filter (input:string) =
    input.Split "\r\n"
    |> Seq.map parseLine
    |> Seq.filter filter
    |> Seq.collect (pixels >> Set.toList)
    |> Seq.groupBy id
    |> Seq.filter (snd >> Seq.length >> (<)1)
    |> Seq.length

let solve  = genericSolve (isDiagonal >> not)
let solve2 = genericSolve (fun _ -> true)

