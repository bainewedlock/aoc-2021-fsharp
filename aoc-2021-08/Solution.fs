module Solution


type Entry = {
    signals : string list 
    outputs : string list }

type DecodeState = {
    decoded : Map<int, string>
    todo    : string list }

let trim (s:string) = s.Trim()
let split (d:string) (s:string) =
    s.Split(d) |> Array.toList |> List.map trim

let parse (line:string) =
    let [left;right] = split "|" line
    {
        signals = split " " left
        outputs = split " " right
    }

let simpleCodes = [ 2,1; 3,7; 4,4; 7,8]
let simpleLengths = simpleCodes |> List.map fst |> Set

let eval (xs:string list) =
    xs
    |> List.filter (fun x -> simpleLengths.Contains x.Length)
    |> List.length

let identify state digit code =
    { state with
        decoded = state.decoded.Add(digit, code)
        todo    = state.todo |> List.except [code] }

let findWithLength l =
    List.filter (fun (x:string) -> x.Length = l)
    >> List.exactlyOne

let decodeSimple codes =
    simpleCodes
    |> List.fold (fun s (l, digit) ->
        let code = findWithLength l s.todo
        identify s digit code) { decoded = Map.empty; todo = codes }

let countMatchingChars (a:string) (b:string) =
    a
    |> Seq.filter (fun c -> b.Contains(c))
    |> Seq.length

let findCode state length existingDigit matchingChars =
    let code = state.decoded.Item existingDigit
    state.todo
    |> List.filter (fun x ->
        countMatchingChars x code = matchingChars &&
        x.Length =  length)
    |> List.exactlyOne

let decodeSingle digit length existingDigit matchingChars state =
    findCode state length existingDigit matchingChars
    |> identify state digit

let decodeComplex (state:DecodeState) =
    state
    |> decodeSingle 3 5 7 3
    |> decodeSingle 5 5 4 3
    |> fun s -> identify s 2 (findWithLength 5 s.todo)
    |> decodeSingle 6 6 7 2
    |> decodeSingle 9 6 5 5
    |> fun s -> identify s 0 (s.todo |> List.exactlyOne)

let sortLetters (s:string) =
    s.ToCharArray()
    |> Array.sort
    |> System.String

let buildMap (m:Map<int,string>) =
    m
    |> Seq.map (fun k -> sortLetters k.Value, k.Key)
    |> Map

let decode entry =
    let decoded = 
        entry.signals
        |> decodeSimple
        |> decodeComplex
        |> fun s -> s.decoded
        |> buildMap
    entry.outputs
    |> List.map (fun x -> decoded.Item (sortLetters x) |> string)
    |> String.concat ""
    |> int

let solve input =
    input
    |> split "\r\n"
    |> List.map parse
    |> List.map (fun e -> eval e.outputs)
    |> List.sum

let solve2 (input:string) = 
    input.Split "\r\n"
    |> Array.toList
    |> List.sumBy (parse >> decode)
