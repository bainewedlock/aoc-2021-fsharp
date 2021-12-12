module Solution

open System

type Cave =
    | StartCave
    | BigCave of string
    | SmallCave of string
    | EndCave

let parseCave = function
    | "start" -> StartCave
    | "end"   -> EndCave
    | x when Char.IsLower(x.[0]) -> SmallCave x
    | x                          -> BigCave x

let tupleFromArray = function
    | [|a;b|] -> a,b
    | _       -> failwith "unexpected"

let parseLine (line:string) =
    line.Trim().Split '-'
    |> Array.map parseCave
    |> tupleFromArray

let parse (input:string) =
    input.Split "\r\n"
    |> Array.toList
    |> List.map parseLine

type Link = Cave * Cave

type Map = {
    currentCave : Cave
    path : Cave List
    remainingCaves : Cave Set 
    visitCount : Map<Cave, int>
    links : Link Set
}

let isSmallCave = function
    | SmallCave _ -> true
    | _           -> false

let isStartCave = function
    | StartCave -> true
    | _         -> false

let isBigCave = function
    | BigCave _ -> true
    | _         -> false

let setupMap (links:Link List) =
    let remaining =
        links
        |> List.collect (fun (a,b) -> [a;b])
        |> List.filter (isStartCave >> not)
        |> Set
    let revLinks =
        links
        |> List.map (fun (a,b) -> b, a)
    let visits =
        remaining
        |> Seq.map (fun c -> c, 0)
        |> Map
    {
        currentCave = StartCave
        path = [StartCave]
        remainingCaves = remaining
        links = List.append links revLinks |> Set
        visitCount = visits
    }

let options smallCaveLimit map = [
    if map.currentCave = EndCave then () else
    let linkedCaves =
        map.remainingCaves
        |> Seq.filter (fun c -> map.links.Contains (map.currentCave, c))
    for c in linkedCaves do
        let alreadyDouble =
            map.visitCount
            |> Map.exists (fun k v -> isSmallCave k &&  v > 1)
        let visitCount = map.visitCount.Item c + 1
        if isSmallCave c && visitCount>1 &&
            (smallCaveLimit=1 || alreadyDouble) then () else
        let remaining =
            match c with
            | SmallCave _ -> map.remainingCaves
            | BigCave _ -> map.remainingCaves
            | _         -> map.remainingCaves.Remove c
        yield { map with
                    currentCave = c
                    path = c::map.path
                    remainingCaves = remaining
                    visitCount = map.visitCount.Add(c, visitCount)
        }
]


let paths smallCaveLimit (links:Link list) =
    let rec loop map = [
        for o in options smallCaveLimit map do
            if o.currentCave = EndCave
            then yield o
            else yield! loop o
    ]
    loop (setupMap links)
    

let solve (input:string) =
    input
    |> parse
    |> paths 1
    |> Seq.length

let debug input =
    input
    |> parse
    |> paths 2
    |> List.map (fun m ->
        m.path
        |> List.rev
        |> List.map (function 
            | SmallCave x -> x
            | BigCave x -> x
            | EndCave -> "end"
            | StartCave -> "start")
        |> String.concat ",")



let solve2 (input:string) =
    input
    |> parse
    |> paths 2
    |> Seq.length

