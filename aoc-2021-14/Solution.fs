module Solution

type Pair = string
type Input = { template : string; rules : Map<string, string> }

let parseLine (line:string) =
    match line.Split " -> " with
    | [|left;right|] -> left, right
    | _              -> failwithf "unexpected line: %s" line

let parseInput (input:string) =
    let lines = input.Split "\r\n" |> Array.toList
    {
        template = lines.[0]
        rules = lines |> List.skip 2 |> List.map parseLine |> Map
    }

let left  (s:string) = s.Substring(0,1)
let right (s:string) = s.Substring(1,1)

let step n input =
    let replace (s,cnt) =
        match input.rules.TryFind s with
        | None   -> [s, cnt]
        | Some x -> [left s + x,  cnt
                     x + right s, cnt]
    let rec loop i counts =
        if i = 0 then counts else
        counts
        |> Seq.collect replace
        |> Seq.groupBy fst
        |> Seq.map (fun (s,cs) -> s, Seq.sumBy snd cs)
        |> loop (i-1)
    input.template
    |> Seq.map string
    |> Seq.pairwise
    |> Seq.map (fun (a,b) -> a+b, 1UL)
    |> loop n

let genericSolve n input =
    let setup = parseInput ("!" + input) // put 1 extra char at the start
    let counts =
        step n setup
        |> Seq.map (fun (s, cnt) -> right s, cnt)
        |> Seq.groupBy fst
        |> Seq.map (snd >> Seq.sumBy snd)
    (Seq.max counts) - (Seq.min counts)
    |> sprintf "%d"

let solve  = genericSolve 10
let solve2 = genericSolve 40
