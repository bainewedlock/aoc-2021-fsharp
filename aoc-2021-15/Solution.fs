module Solution

open System.Collections.Generic
open System.Diagnostics
open System

type Point = int * int

type Grid = {
    cells : IDictionary<Point, int>
    pos   : Point
    visited : Point Set
    risk : int
    goal  : Point
    width : int
    height : int
}

type Risk = int
type Graph = Map<Point, Risk>

let parse (input:string) =
    let cells =
        input.Split "\r\n"
        |> Seq.indexed
        |> Seq.collect (fun (y,line) ->
            Seq.indexed line
            |> Seq.map (fun (x, c) -> (x, y), int (string c)))
    let mx = cells |> Seq.map (fst >> fst) |> Seq.max
    let my = cells |> Seq.map (fst >> snd) |> Seq.max
    {
        cells = Map cells
        pos   = (0,0)
        visited = Set [(0,0)]
        risk = 0
        goal = (mx,my)
        width = mx+1
        height = my+1
    }

let orthogonal = [1,0; 0,1; -1,0; 0,-1]

let options grid =
    orthogonal
    |> List.map (fun (dx,dy) -> (dx + fst grid.pos, dy + snd grid.pos))
    |> List.filter grid.cells.ContainsKey

let dijkstra grid =
    let mutable risk = -1
    let Q = HashSet<Point>()
    let dist = Dictionary<Point, Risk>()
    let prev = Dictionary<Point, Point Option>()

    for x in grid.cells do
        dist.Add(x.Key, Int32.MaxValue)
        prev.Add(x.Key, None)
        Q.Add(x.Key) |> ignore
    dist.Item grid.pos <- 0

    while Q.Count > 0 do
        let u = Q |> Seq.minBy (fun u -> dist.Item u)

        Q.Remove u |> ignore

        if u = grid.goal then
            Q.Clear()
            if (prev.Item u).IsSome || u = grid.pos then
                let mutable S = []
                let mutable u = Some u
                while u.IsSome do
                    S <- u.Value::S
                    u <- prev.Item (u.Value)
                risk <- S |> List.sumBy (fun p -> grid.cells.Item p)
                risk <- risk - (grid.cells.Item grid.pos)
        else
            let neighbours =
                options { grid with pos = u }
                |> List.filter (fun v ->  Q.Contains v)
            for v in neighbours do
                // add both points risk because direction should not matter?
                let alt =
                    dist.Item u +
                    grid.cells.Item v +
                    grid.cells.Item u
                if alt < dist.Item v then
                    dist.Item v <- alt
                    prev.Item v <- Some u
    risk


let duplicate grid =
    let rec loop i = seq [
        if i = 0 then () else
        let fx = i % 5
        let fy = i / 5
        yield!
            grid.cells
            |> Seq.map (fun p ->
                let x0, y0 = p.Key
                ((x0 + fx * grid.width, y0 + fy * grid.height), p.Value))
        yield! (loop (i-1))
    ]
    loop 25
    |> fun c -> { grid with cells = c |> dict}



let solve input =
    input
    |> parse
    |> dijkstra




let solve2 input =
    input
    |> parse
    |> duplicate
    |> dijkstra






    


