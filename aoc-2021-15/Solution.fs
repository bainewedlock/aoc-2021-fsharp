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

type Path(risk:int, point:Point) =
    member this.risk = risk
    member this.point = point
    interface IComparable<Path> with
        member this.CompareTo(other: Path): int = 
            let comp = this.risk.CompareTo(other.risk)
            if (comp <> 0) then comp else
            (fst other.point + snd other.point).CompareTo(
             fst this.point + snd this.point)

let dijkstra grid =
    let queue = new List<Path>()
    let seen = HashSet<Point>()
    queue.Add(Path(0, (0,0)))
    seen.Add((0,0)) |> ignore
    let rec loop () =
        if queue.Count = 0 then failwithf "unexpected" else
        let current = queue.[0]
        queue.RemoveAt 0
        if current.point = grid.goal then current.risk else
        let visit =
            options { grid with pos = current.point }
            |> List.map (fun p -> Path(current.risk + grid.cells.Item p, p))
            |> List.filter (fun p -> not <| seen.Contains(p.point))
        for v in visit do seen.Add(v.point) |> ignore
        queue.AddRange(visit)
        queue.Sort()
        loop ()
    loop ()

let rec wrap x =
    if x > 9 then wrap (x-9) else
    x

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




    


