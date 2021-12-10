module Solution

type SyntaxEval =
    | Incomplete of string
    | Unexpected of char
    | Valid

let printCharList xs =
    List.map string xs
    |> String.concat ""

let findError (s:string) =
    let rec loop acc str =
        match acc, str with
        // stack empty at end of string
        | [], []        -> Valid
        // stack not empty at end of string
        | acc, []       -> Incomplete (printCharList acc)
        // got opening char: put the closing char on the stack
        | _, '['::stail -> loop (']'::acc) stail
        | _, '('::stail -> loop (')'::acc) stail
        | _, '{'::stail -> loop ('}'::acc) stail
        | _, '<'::stail -> loop ('>'::acc) stail
        // got expected closing char: continue with rest
        | a::atail, s::stail when a=s -> loop atail stail
        // got any other char
        | _::_, s::_                  -> Unexpected s
        // got something completely unexpected
        | _                           -> failwith "boom!"
    loop [] (Seq.toList s)

let scorePart1 = function
    | Unexpected ')' -> 3
    | Unexpected ']' -> 57
    | Unexpected '}' -> 1197
    | Unexpected '>' -> 25137
    | _              -> 0

let tryGetIncomplete = function
    | Incomplete s -> Some s
    | _            -> None

let rec scorePart2 =
    let scoreChar = function
        | ')' -> 1UL
        | ']' -> 2UL
        | '}' -> 3UL
        | '>' -> 4UL
    Seq.fold (fun acc c -> acc * 5UL + scoreChar c) 0UL

let middle xs =
    let l = Seq.length xs
    let i = l / 2
    Seq.item i xs

let solve (input:string) =
    input.Split "\r\n"
    |> Seq.map findError
    |> Seq.sumBy scorePart1

let solve2 (input:string) =
    input.Split "\r\n"
    |> Seq.map findError
    |> Seq.choose tryGetIncomplete
    |> Seq.map scorePart2
    |> Seq.sort
    |> middle
