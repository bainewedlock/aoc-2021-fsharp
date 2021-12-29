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

let addToSet (s:Point Set) (p:Path) = s.Add p.point
let addToHeap (h:Heap<Path>) (p:Path) = h.Insert p

let dijkstra grid =
    let seen = Set [(0,0)]
    let heap = Heap(false, 0, HeapData.E).Insert { risk=0; point=0,0 }
    let rec loop (seen:Point Set) (heap:Heap<Path>) =
        let cur = heap.Head
        if cur.point = grid.goal then cur.risk else
        let visit =
            neighbours cur.point grid.cells
            |> List.map (fun p ->
                { risk=cur.risk + grid.cells.Item p; point = p })
            |> List.filter (fun p -> not <| seen.Contains(p.point))
        let seen' = visit |> List.fold addToSet seen
        let heap' = visit |> List.fold addToHeap (heap.Tail())
        loop seen' heap'
    loop seen heap

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
