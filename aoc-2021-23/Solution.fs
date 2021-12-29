module Solution

open System

type State = char list list
type Cost = int
type Decision = int * State

let parseInput (input:string) =
    let ls = input.Replace("_", "").Split "\r\n"
    let p i = sprintf "%c%c" ls.[2].[i] ls.[3].[i]
    sprintf "||%s|%s|%s|%s||" (p 3) (p 5) (p 7) (p 9)

let parseState (s:string) =
    s.Split "|"
    |> Array.toList
    |> List.map (Seq.toList)

let printState (state:State) =
    state
    |> List.map (Array.ofSeq >> String)
    |> String.concat "|"

let updateAt vi v xs = [
    for i,x in List.indexed xs do
        yield if i=vi then v else x ]

let updateElement key f st = 
  st |> List.map (fun (k, v) -> if k = key then k, f v else k, v)

let isSideroom = function
    | 2 -> true
    | 4 -> true
    | 6 -> true
    | 8 -> true
    | _ -> false

let costFor = function
    | 'A' -> 1
    | 'B' -> 10
    | 'C' -> 100
    | 'D' -> 1000

let noOtherContent c = List.exists ((<>)c) >> not

let allMovesFor dir i (s:string) =
    let state = parseState s
    if state.[i] = [] then [] else
    let pod::rest = state.[i]
    let moveTo x =
        state
        |> updateAt i rest
        |> updateAt x (pod::state.[x])
    let rec loop cost x = [
        if x + dir < 0 then () else
        if x + dir = state.Length then () else
        let x = x + dir
        if noOtherContent pod state.[x] then
            let dc = if isSideroom x then 2 else 1
            yield printState (moveTo x), (cost+dc) * (costFor pod)
        yield! loop (cost+1) x
    ]
    let initialcost = if isSideroom i then 1 else 0
    loop initialcost i

let allRightMovesFor i (s:string) = allMovesFor 1 i s
let allLeftMovesFor i (s:string) = allMovesFor -1 i s

let options (start:string) =
    [
        for i in 0..10 do
            yield! allLeftMovesFor i start
            yield! allRightMovesFor i start
    ]
