module Solution

//    |1 0 0| |x|   |x*1 + y*0 + z*0|   |x'|
//    |0 1 0| |y| = |x*0 + y*1 + z*0| = |y'|
//    |0 0 1| |z|   |x*0 + y*0 + z*1|   |z'|
let apply (matrix:int List List) (vector: int List) = [
    for line in 0..2 do
        let a = vector.[0] * matrix.[line].[0]
        let b = vector.[1] * matrix.[line].[1]
        let c = vector.[2] * matrix.[line].[2]
        yield a+b+c ]

let rotations = [
    [[1;0;0]
     [0;1;0]
     [0;0;1]];
    [[1;0;0]
     [0;0;-1]
     [0;1;0]];
    [[1;0;0]
     [0;-1;0]
     [0;0;-1]];
    [[1;0;0]
     [0;0;1]
     [0;-1;0]];
    [[0;-1;0]
     [1;0;0]
     [0;0;1]];
    [[0;0;1]
     [1;0;0]
     [0;1;0]];
    [[0;1;0]
     [1;0;0]
     [0;0;-1]];
    [[0;0;-1]
     [1;0;0]
     [0;-1;0]];
    [[-1;0;0 ]
     [0;-1;0 ]
     [0;0;1 ]];
    [[-1;0;0 ]
     [0;0;-1 ]
     [0;-1;0 ]];
    [[-1;0;0 ]
     [0;1;0 ]
     [0;0;-1 ]];
    [[-1;0;0 ]
     [0;0;1 ]
     [0;1;0 ]];
    [[0;1;0 ]
     [-1;0;0 ]
     [0;0;1 ]];
    [[0;0;1 ]
     [-1;0;0 ]
     [0;-1;0 ]];
    [[0;-1;0 ]
     [-1;0;0 ]
     [0;0;-1 ]];
    [[0;0;-1 ]
     [-1;0;0 ]
     [0;1;0 ]];
    [[0;0;-1 ]
     [0;1;0 ]
     [1;0;0 ]];
    [[0;1;0 ]
     [0;0;1 ]
     [1;0;0 ]]
    [[0;0;1 ]
     [0;-1;0 ]
     [1;0;0 ]]
    [[0;-1;0 ]
     [0;0;-1 ]
     [1;0;0 ]]
    [[0;0;-1 ]
     [0;-1;0 ]
     [-1;0;0 ]]
    [[0;-1;0 ]
     [0;0;1 ]
     [-1;0;0 ]]
    [[0;0;1 ]
     [0;1;0 ]
     [-1;0;0 ]];
    [[0;1;0 ]
     [0;0;-1 ]
     [-1;0;0]] ]

let parse (input:string) =
    let parseLine (line:string) =
        line.Split ","
        |> Array.toList
        |> List.map int
    let folder (scanner::rest) (line:string) =
        if line.Trim() = ""
        then []::scanner::rest
        else (parseLine line::scanner)::rest
    input.Split "\r\n"
    |> Seq.filter (fun l -> l.Contains "scanner" |> not)
    |> Seq.rev
    |> Seq.fold folder [[]]

type Point = int list
type Scanner = Point list

let rotateVectors vs m = vs |> List.map (apply m)

// combinations: https://stackoverflow.com/questions/1222185/most-elegant-combinations-of-elements-in-f
let rec comb n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

let overlap (ps1:Point list) (ps2:Point list) =
    []


let overlapScanners (s1:Scanner) (s2:Scanner) =
    rotations
    |> List.map (rotateVectors s2)
    |> List.map (fun s2' -> Set.intersect (Set s1) (Set s2'))
    |> List.maxBy Set.count
