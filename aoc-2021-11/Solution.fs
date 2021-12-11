module Solution

let threshold = 9
type Pos = int * int
type Grid = Map<Pos,int>

let parseChar y (x,c) =
    (x,y), (c |> string |> int)

let parseLine (y,line:string) =
    line.Trim()
    |> Seq.indexed
    |> Seq.map (parseChar y)

let parse (input:string) =
    input.Split "\r\n"
    |> Seq.indexed
    |> Seq.collect parseLine
    |> Map

let rec increaseAt (x,y) (grid:Grid) =
    match grid.TryFind (x,y) with
    | None -> grid
    | Some initialEnergy ->
        let grid =
            grid |> Map.add (x,y) (1 + initialEnergy)
        if initialEnergy <> threshold then grid else
        grid
        |> increaseAt (x+1, y)
        |> increaseAt (x-1, y)
        |> increaseAt (x, y+1)
        |> increaseAt (x, y-1)
        |> increaseAt (x+1, y+1)
        |> increaseAt (x+1, y-1)
        |> increaseAt (x-1, y+1)
        |> increaseAt (x-1, y-1)

let increaseEnergy (grid:Grid) =
    grid
    |> Map.fold (fun g k _ -> increaseAt k g) grid

let step =
    increaseEnergy
    >> Seq.fold (fun (flashes, grid) x ->
        let f, v =
            if x.Value > threshold 
            then 1, 0
            else 0, x.Value
        f+flashes, Map.add x.Key v grid) (0, Map.empty)

let steps n (grid:Grid) =
    let rec loop n acc grid =
        if n = 0 then acc, grid else
        let flashes, grid2 = step grid
        loop (n-1) (acc+flashes) grid2
    loop n 0 grid

let solve = parse >> steps 100 >> fst

let isSynchronized = Map.forall (fun _ v -> v = 0)

let rec synchronize n (g:Grid) =
    if isSynchronized g then n else
    let _, g2 = step g
    synchronize (n+1) g2

let solve2 = parse >> synchronize 0
