module Solution

open System.Collections.Generic
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

let rec heappush v xs = [
    match xs with
    | x::rest when x <= v ->
        yield x
        yield! heappush v rest
    | rest ->
        yield v
        yield! rest ]

type Prev = Dictionary<Point, Point Option>

type MyQueue = (Risk * Point) list

let dijkstra grid =
    let risk = Dictionary<Point, Risk>()

    let isBetterAlternative foundRisk atPoint =
        match risk.TryGetValue atPoint with
        | true, r when r <= foundRisk -> false
        | _                           -> true

    // keep searching notes with next smallest distance
    let rec loop = function
        | [] -> failwithf "unexpected"
        | (ur,u)::queueRest ->
            if u = grid.goal then () else // found goal, exit
            let explorePoints =
                options { grid with pos = u }
                |> List.map (fun p -> (ur + grid.cells.Item p, p))
                |> List.filter (fun (r,p) -> isBetterAlternative r p)
            let queue2 =
                explorePoints
                |> List.fold (fun q p -> heappush p q) queueRest
            for r,p in explorePoints do
                risk.Item p <- r
            loop queue2
    loop [0, grid.pos]
    risk.Item grid.goal

let multiplyGrid grid =
    let rec loop i = seq [
        if i = -1 then () else
        let fx = i % 5
        let fy = i / 5
        yield!
            grid.cells
            |> Seq.map (fun p ->
                let x0, y0 = p.Key
                ((x0 + fx * grid.width, y0 + fy * grid.height), p.Value))
        yield! (loop (i-1))
    ]
    loop 24
    |> fun c -> { grid with cells = c |> dict; goal = (499, 499) }

let solve input =
    input
    |> parse
    |> dijkstra

let solve2 input =
    input
    |> parse
    |> multiplyGrid
    |> dijkstra




    


