module Solution

open System.Text.RegularExpressions

type Command = On | Off

type Cube = {
    x0 : int
    x1 : int
    y0 : int
    y1 : int
    z0 : int
    z1 : int }

type Borders = {
    x : int Set
    y : int Set
    z : int Set }

let parseCommand (line:string) =
    match line.Split " " |> Array.head with
    | "on"  -> On
    | "off" -> Off
    | _ -> failwithf "unexpected line: %s" line

let parse (line:string) =
    Regex.Matches(line, @"\-?\d+")
    |> Seq.map (fun m -> int m.Value)
    |> Seq.toList
    |> List.chunkBySize 2
    |> List.map List.sort
    |> function
    | [[x0;x1];[y0;y1];[z0;z1]] -> 
        (parseCommand line),
        {
            x0 = x0
            x1 = x1
            y0 = y0
            y1 = y1
            z0 = z0
            z1 = z1
        }

let borders (c:Cube seq) =
    {
        x = c |> Seq.collect (fun c -> [c.x0;c.x1+1]) |> Set
        y = c |> Seq.collect (fun c -> [c.y0;c.y1+1]) |> Set
        z = c |> Seq.collect (fun c -> [c.z0;c.z1+1]) |> Set    
    }

let foldBorders f init (borders:Borders) =
    let mutable acc = init
    for x0,x1 in borders.x |> Seq.pairwise do
        for y0,y1 in borders.y |> Seq.pairwise do
            for z0,z1 in borders.z |> Seq.pairwise do
                acc <- f acc {
                        x0 = x0
                        x1 = x1
                        y0 = y0
                        y1 = y1
                        z0 = z0
                        z1 = z1
                    }
    acc

let area c = 
    (uint64 (c.x1-c.x0)) *
    (uint64 (c.y1-c.y0)) * 
    (uint64 (c.z1-c.z0))

let genericSolve append fFilter (input:string) = 
    let input = input.Split "\r\n" |> Seq.map parse
    let cache = input |> Seq.rev |> Seq.toList
    let get b =
        cache
        |> List.tryPick (fun (cmd, c) -> 
            if b.x0 >= c.x0 && b.x0 <= c.x1 &&
               b.y0 >= c.y0 && b.y0 <= c.y1 &&
               b.z0 >= c.z0 && b.z0 <= c.z1
            then Some cmd
            else None)
        |> function
        | Some On  -> area b
        | _        -> 0UL
    input
    |> Seq.map snd
    |> Seq.append append
    |> Seq.filter fFilter
    |> borders
    |> foldBorders (fun (cnt,sum) b -> (cnt+1), (sum+get b)) (0, 0UL)
    |> snd

let part1cube = { x0 = -50; x1 = 50; y0 = -50; y1 = 50; z0 = -50; z1 = 50 }
let part1filter b =
    (b.x0 >= -50 && b.x0 <= 50 || b.x1 >= -50 && b.x1 <= 50) &&
    (b.y0 >= -50 && b.y0 <= 50 || b.y1 >= -50 && b.y1 <= 50) &&
    (b.z0 >= -50 && b.z0 <= 50 || b.z1 >= -50 && b.z1 <= 50)

let solve = genericSolve [part1cube] part1filter
let solve2 = genericSolve [] (fun _ -> true)

