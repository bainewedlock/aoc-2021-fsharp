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
    z : int Set
}

let parseCommand (line:string) =
    match line.Split " " |> Array.head with
    | "on"  -> On
    | "off" -> Off

let parse (line:string) =
    Regex.Matches(line, @"\-?\d+")
    |> Seq.map (fun m -> int m.Value)
    |> Seq.toList
    |> List.chunkBySize 2
    |> List.map List.sort
    |> function
    | [[x0;x1];[y0;y1];[z0;z1]] -> 
        (parseCommand line), {
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

let mapBoxes f (borders:Borders) = seq [
    let mutable i = 0
    for (x0,x1) in borders.x |> Seq.pairwise do
    for (y0,y1) in borders.y |> Seq.pairwise do
    for (z0,z1) in borders.z |> Seq.pairwise do
        i <- i + 1
        if i%1000000=0 then printfn "%d" i
        yield f
            {
                x0 = x0
                x1 = x1
                y0 = y0
                y1 = y1
                z0 = z0
                z1 = z1
            }
]
let foldBoxes f (init:int*uint64) (borders:Borders) =
    let mutable acc = init
    for (x0,x1) in borders.x |> Seq.pairwise do
        for (y0,y1) in borders.y |> Seq.pairwise do
            for (z0,z1) in borders.z |> Seq.pairwise do
                acc <- f acc {
                        x0 = x0
                        x1 = x1
                        y0 = y0
                        y1 = y1
                        z0 = z0
                        z1 = z1
                    }
    acc
    

let area c = (c.x1-c.x0) * (c.y1-c.y0) * (c.z1-c.z0)

let solve (input:string) = 
    let input = input.Split "\r\n" |> Seq.map parse

    let get x y z =
        input
        |> Seq.rev
        |> Seq.tryPick (fun (cmd, c) -> 
            if 
                x >= c.x0 && x <= c.x1 &&
                y >= c.y0 && y <= c.y1 &&
                z >= c.z0 && z <= c.z1
            then Some cmd
            else None)

    input
    |> Seq.map snd
    |> Seq.append [
        { x0 = -50; x1 = 50; y0 = -50; y1 = 50; z0 = -50; z1 = 50 } ]
    |> Seq.filter (fun b ->
        (b.x0 >= -50 && b.x0 <= 50 || b.x1 >= -50 && b.x1 <= 50) &&
        (b.y0 >= -50 && b.y0 <= 50 || b.y1 >= -50 && b.y1 <= 50) &&
        (b.z0 >= -50 && b.z0 <= 50 || b.z1 >= -50 && b.z1 <= 50))
    |> borders
    |> mapBoxes id
    |> Seq.filter (fun b ->
        b.x0 >= -50 && b.x0 <= 50 &&
        b.y0 >= -50 && b.y0 <= 50 &&
        b.z0 >= -50 && b.z0 <= 50)
    |> Seq.sumBy (fun b ->
        match get b.x0 b.y0 b.z0 with
        | Some On  -> area b
        | _        -> 0)


let area2 c = 
    (uint64 (c.x1-c.x0)) *
    (uint64 (c.y1-c.y0)) * 
    (uint64 (c.z1-c.z0))

type Point = int * int * int

let solve2 (input:string) = 
    let input = input.Split "\r\n" |> Seq.map parse
    let cache = input |> Seq.rev |> Seq.toList
    let get x y z =
        cache
        |> List.tryPick (fun (cmd, c) -> 
            if 
                x >= c.x0 && x <= c.x1 &&
                y >= c.y0 && y <= c.y1 &&
                z >= c.z0 && z <= c.z1
            then Some cmd
            else None)

    let get2 b =
        match get b.x0 b.y0 b.z0 with
        | Some On  -> area2 b
        | _        -> 0UL

    let folder (cnt, sum) b =
        (cnt+1), (sum+get2 b)

    input
    |> Seq.map snd
    |> borders
    |> foldBoxes folder (0, 0UL)
    |> snd
    //input
    //|> Seq.map snd
    //|> borders
    //|> mapBoxes (fun b -> 
    //    match get b.x0 b.y0 b.z0 with
    //    | Some On  -> area2 b
    //    | _        -> 0UL)
    //|> Seq.sum
