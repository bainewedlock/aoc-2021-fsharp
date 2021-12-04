module Solution

open System.Text.RegularExpressions

type Board = {
    history : int list
    lists : int Set list
    bingo : bool }

let parseBoard (input:string) =
    let ns =
        Regex.Split(input.Trim(), "\s+")
        |> Array.toList
        |> List.map int
        |> List.chunkBySize 5
    let rows = ns |> List.map Set.ofList
    let cols = ns |> List.transpose |> List.map Set.ofList
    {
        history = []
        lists = rows @ cols
        bingo = false
    }

let addNumber n b =
    let lists = b.lists |> List.map (Set.remove n)
    { b with
        history = n::b.history
        lists = lists
        bingo = lists |> List.exists Set.isEmpty
    }

let parse (input:string) =
    let lines = input.Split "\r\n" |> Array.toList
    let header = lines.[0].Split ',' |> Array.toList |> List.map int
    let boards =
        lines
        |> List.skip 2
        |> List.chunkBySize 6
        |> List.map (String.concat " " >> parseBoard)
    header, boards

let rec play numbers board =
    match numbers with
    | _ when board.bingo -> board
    | []                 -> board
    | n::rest            -> play rest (addNumber n board)

let eval b =
    let sum =
        b.lists
        |> List.map Set.toList
        |> List.concat
        |> List.distinct
        |> List.sum
    sum * b.history.Head

let genericSolve fRank input =
    let numbers, boards = parse input
    boards
    |> List.map (play numbers)
    |> fRank (fun b -> b.history.Length)
    |> eval

let solve  = genericSolve List.minBy
let solve2 = genericSolve List.maxBy
