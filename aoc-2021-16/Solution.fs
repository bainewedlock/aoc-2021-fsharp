module Solution

open System

type LengthType =
    | Fixed of int
    | Count of int

type State = {
    data : string
    lengthType : LengthType List
    versions : string List
}

let parse (input:string) =
    input
    |> Seq.map (fun c -> Convert.ToInt32(string c, 16))
    |> Seq.map (fun x -> Convert.ToString(x, 2).PadLeft(4, '0'))
    |> String.concat ""

let bin2int (s:string) = Convert.ToInt32(s, 2)
let bin2int64 (s:string) = Convert.ToUInt64(s, 2)

type Token =
    | Version    of int
    | Literal    of uint64
    | OperatorNo of int

let rec describe (bits:string) = [
    if bits.Length < 11 then () else
    yield (Version (bits.Substring(0,3) |> bin2int))
    let dataType = bits.Substring(3,3) |> bin2int
    if dataType = 4 then
        let rec extractNumbers (s:string) = [
            yield s.Substring(1,4)
            if s.[0] = '1' then yield! extractNumbers (s.Substring 5) ]
        let numbers = extractNumbers (bits.Substring 6)
        yield (Literal (numbers |> String.concat "" |> bin2int64))
        let offset = 6 + numbers.Length * 5
        yield! describe (bits.Substring offset)
    else
        let l =
            match bits.[6] with
            | '0' -> 15
            | '1' -> 11
        let offset = 7 + l
        yield (OperatorNo dataType)
        yield! describe (bits.Substring offset)
]

let solve (input:string) =
    input
    |> parse
    |> describe
    |> List.choose (function
        | Version x -> Some x
        | _         -> None )
    |> List.sum


