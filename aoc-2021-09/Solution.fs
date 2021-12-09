module Solution

let parseChar (y:int) (x:int, c:char) =
    (x,y), c |> string |> int

let parseLine (y:int,line:string) =
    line
    |> Seq.indexed
    |> Seq.map (fun (x,c) -> parseChar y (x, c))

type Pos = int * int
type Grid = Map<Pos, int>

let parse (input:string) =
    input.Split "\r\n"
    |> Seq.indexed
    |> Seq.collect parseLine
    |> Map

let adjacent (x,y) = [
    x+1,y+0
    x-1,y+0
    x+0,y+1
    x+0,y-1 ]

let lowpoints (grid:Grid) =
    grid
    |> Seq.choose (fun x ->
        let min =
            adjacent x.Key
            |> List.choose (fun p -> grid.TryFind p)
            |> List.min
        if min > x.Value then Some (x.Key, x.Value) else None )
    |> Seq.toList

let solve (input:string) =
    input
    |> parse
    |> lowpoints
    |> List.sumBy (snd >> (+)1)

let getNon9Positions g =
    Map.filter (fun _ v -> v <> 9) g
    |> Seq.map (fun k -> k.Key) |> Set

let extractBasin (ps:Pos Set) =
    let somePoint = Seq.head ps
    let rec loop acc (check:Pos Set) (remaining:Pos Set) =
        if check.IsEmpty || remaining.IsEmpty then acc, remaining else
        let ns =
            check
            |> Seq.collect adjacent
            |> Set
            |> Set.intersect remaining
        let check2 = Set.union acc ns
        let remaining2 = Set.difference remaining ns
        loop check2 ns remaining2
    loop Set.empty (Set [somePoint]) ps

let solve2 (input:string) =
    let g = input |> parse |> getNon9Positions
    let rec loop acc (h2:Pos Set) =
        if h2.IsEmpty then acc else
        let b,h3 = extractBasin h2
        loop (b::acc) h3
    loop [] g
    |> List.map (fun x -> x.Count)
    |> List.sortDescending
    |> List.take 3
    |> List.reduce (*)
