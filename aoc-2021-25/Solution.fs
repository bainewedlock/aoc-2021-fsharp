module Solution
open System

type Sea = char[,]

let split (delimiter:string) (text:string) =
    text.Split(delimiter) |> Array.toList

let trim (text:string) = text.Trim()

let parse (input:string) =
    let lines = split "\r\n" input |> List.map trim
    let w = lines.[0].Length
    let h = lines.Length
    let f x y = lines.[y].[x]
    Array2D.init w h f

let width (sea:Sea) = sea.GetLength 0
let height (sea:Sea) = sea.GetLength 1
let maxX (sea:Sea) = width sea - 1
let maxY (sea:Sea) = height sea - 1

let printLine (sea:Sea) y =
    [|0..maxX sea|]
    |> Array.map (fun x -> sea.[x,y]) 
    |> String

let print (sea:Sea) =
    [0..maxY sea]
    |> List.map (printLine sea)
    |> String.concat "\r\n"

let move c dx dy (sea:Sea) =
    let before = Array2D.copy sea
    let tryMove x1 y1 x2 y2 =
        let x2 = x2 % width sea
        let y2 = y2 % height sea
        if before.[x1,y1] = c && before.[x2,y2] = '.' then
            sea.[x1,y1] <- '.'
            sea.[x2,y2] <- c
    for x in 0..maxX sea do
    for y in 0..maxY sea do
        tryMove x y (x+dx) (y+dy)

let tick (sea:Sea) =
    move '>' 1 0 sea
    move 'v' 0 1 sea

let solve input : int =
    let sea = parse input
    let rec loop n =
        let prev = Array2D.copy sea
        tick sea
        if sea = prev then n else
        loop (n+1)
    loop 1
