module Solution

open System.Collections.Generic
open FSharpx.Collections

type Point = int * int
type Cells = IDictionary<Point, int>

type Input = {
    cells : Cells
    pos   : Point
    goal  : Point
    width : int
    height : int }

let parseLine (y:int,line:string) =
    let parseChar (x, c) = (x,y), c|>string|>int
    Seq.indexed line |> Seq.map parseChar

let parse (input:string) =
    let cells = input.Split "\r\n" |> Seq.indexed |> Seq.collect parseLine
    let mx = cells |> Seq.map (fst >> fst) |> Seq.max
    let my = cells |> Seq.map (fst >> snd) |> Seq.max
    {
        cells  = Map cells
        pos    = 0,0
        goal   = mx,my
        width  = mx+1
        height = my+1
    }

let neighbours (x,y) (cells:Cells) =
    [1,0; 0,1; -1,0; 0,-1]
    |> List.map (fun (dx,dy) -> (dx + x, dy + y))
    |> List.filter cells.ContainsKey

type Path = { risk : int; point : Point }

let dijkstra grid =
    let seen = HashSet<Point>()
    let queue = [{ risk=0; point=0,0 }]
    let heap = Heap<Path>(false, 0, HeapData.E)
    let heap = heap.Insert { risk=0; point=0,0 }
    seen.Add((0,0)) |> ignore
    let rec loop (heap:Heap<Path>) =
        if queue = [] then failwithf "unexpected" else
        let current = heap.Head
        if current.point = grid.goal then current.risk else
        let visit =
            neighbours current.point grid.cells
            |> List.map (fun p ->
                { risk=current.risk + grid.cells.Item p; point = p })
            |> List.filter (fun p -> not <| seen.Contains(p.point))
        for p in visit do seen.Add(p.point) |> ignore
        visit
        |> List.fold (fun (h:Heap<Path>) v -> h.Insert v) (heap.Tail())
        |> loop
    loop heap

let rec wrap x = if x > 9 then wrap (x-9) else x

let multiplyGrid grid =
    let cells = seq [
        for fx in 0..4 do
        for fy in 0..4 do
        for c in grid.cells do
            let x0, y0 = c.Key
            let p = x0 + fx*grid.width, y0+fy*grid.height
            let v = c.Value + fx + fy |> wrap
            yield p, v ]
    let u = grid.width * 5 - 1
    { grid with cells = dict cells; goal = (u,u) }

let solve input =
    input
    |> parse
    |> dijkstra

let solve2 input =
    input
    |> parse
    |> multiplyGrid
    |> dijkstra
