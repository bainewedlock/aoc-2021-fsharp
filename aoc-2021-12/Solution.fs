module Solution

open System

type Cave =
    | StartCave
    | BigCave of string
    | SmallCave of string
    | EndCave

type Link = Cave * Cave

type Map = {
    currentCave : Cave
    path : Cave List
    links : Link Set
    remaining : Map<Cave, int> }

let defaultMap =
    {
        currentCave = StartCave
        path        = [StartCave]
        links       = Set.empty
        remaining   = Map.empty
    }

let parseCave = function
    | "start"                    -> StartCave
    | "end"                      -> EndCave
    | x when Char.IsLower(x.[0]) -> SmallCave x
    | x                          -> BigCave x

let parseLine (line:string) =
    let tuple = function
        | [|a;b|] -> a,b
        | _       -> failwith "unexpected"
    line.Trim().Split '-'
    |> Array.map parseCave
    |> tuple

let parse (input:string) =
    input.Split "\r\n"
    |> Array.toList
    |> List.map parseLine

let setupMap smallCaveLimit (links:Link List) =
    let setupCave = function
        | StartCave   -> None
        | SmallCave x -> Some (SmallCave x, smallCaveLimit)
        | x           -> Some (x, 1)
    let swap (a,b) = b,a
    let revLinks = List.map swap links
    let toList (a,b) = [a;b]
    { defaultMap with
        links = Set (links @ revLinks)
        remaining =
            links
            |> List.collect toList
            |> List.choose setupCave 
            |> Map
    }

let reduceSpecificCave c (remaining:Map<Cave, int>) =
    let f value =
        match c, value with
        | BigCave _, Some x -> Some x
        | _,         Some 1 -> None
        | _,         Some x -> Some (x-1)
        | _,         None   -> None
    remaining.Change(c, f)

let reduceAllSmallCaves (remaining:Map<Cave, int>) =
    let f = function
        | SmallCave _, 1 -> None
        | SmallCave x, v -> Some (SmallCave x, v-1)
        | x              -> Some x
    remaining |> Map.toSeq |> Seq.choose f |> Map

let visitCount c (map:Map) =
    map.path
    |> List.filter ((=)c)
    |> List.length
    |> (+)1

let linkedCaves (map:Map) =
    map.remaining
    |> Seq.toList
    |> List.choose (fun m ->
        if map.links.Contains (map.currentCave, m.Key)
        then Some m.Key else None)

let remaining c (map:Map) =
    match c, visitCount c map with
    | SmallCave _, x when x > 1 -> reduceAllSmallCaves map.remaining
    | _                         -> reduceSpecificCave c map.remaining
 
let gotoCave map c =
    { map with
        currentCave = c
        path = c::map.path
        remaining = remaining c map }

let options map =
    if map.currentCave = EndCave then [] else
    linkedCaves map
    |> List.map (gotoCave map)

let rec paths map = [
    for o in options map do
        if o.currentCave = EndCave
        then yield o
        else yield! paths o ]

let genericSolve smallCaveLimit (input:string) =
    input
    |> parse
    |> setupMap smallCaveLimit
    |> paths
    |> Seq.length

let solve  = genericSolve 1
let solve2 = genericSolve 2
