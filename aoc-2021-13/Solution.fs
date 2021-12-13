module Solution

type Point = int * int
type Fold  = FoldY of int | FoldX of int
type Instructions = { points : Point Set; folds  : Fold List }

let split delimiters (s:string) =
    s.Split(List.toArray delimiters) |> Array.toList

let addFold f ins = { ins with folds = f::ins.folds }

let parseLine ins =
    split [',';'='] >> function
    | [f;x] when f.EndsWith "x" -> ins |> addFold (FoldX (int x))
    | [f;y] when f.EndsWith "y" -> ins |> addFold (FoldY (int y))
    | [sx;sy] -> { ins with points = ins.points.Add (int sx, int sy) }
    | _ -> ins

let foldPoints = function
    | FoldX fx -> Set.map (fun (x,y) -> if x < fx then x,y else (2*fx-x, y))
    | FoldY fy -> Set.map (fun (x,y) -> if y < fy then x,y else (x, 2*fy-y))

let emptyInstructions = { points = Set.empty; folds = [] }

let parse (input:string) =
    input.Split "\r\n"
    |> Seq.rev
    |> Seq.fold parseLine emptyInstructions

let solve input =
    let ins = parse input
    foldPoints ins.folds.Head ins.points
    |> Set.count

let print (ps:Point Set) =
    let w = ps |> Seq.map fst |> Seq.max |> (+)1
    let h = ps |> Seq.map snd |> Seq.max |> (+)1
    for y in 0..h do
        for x in 0..w do
            if ps.Contains (x,y)
            then printf "██"
            else (printf "  ")
        printfn ""

let solve2 (input:string) =
    let ins = parse input
    ins.folds
    |> Seq.fold (fun ps f -> foldPoints f ps) ins.points
    |> print


