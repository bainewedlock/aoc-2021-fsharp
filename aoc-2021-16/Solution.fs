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
    |> fun d -> { data = d; lengthType = [Count 1]; versions = [] }

let decrease offset = function
    | [] -> []
    | Count 1::rest                 -> rest
    | Count x::rest                 -> Count (x-1)::rest
    | Fixed x::rest when x = offset -> rest
    | Fixed x::rest                 -> Fixed (x-offset)::rest

let bin2int (s:string) = Convert.ToInt32(s, 2)

let step state =
    if state.data.Length < 11 then { state with data = "" } else
    let version = state.data.Substring(0,3)
    let dataType = state.data.Substring(3,3)
    if dataType = "100" then
        let rec extractNumbers (s:string) = [
            yield s.Substring(1,4)
            if s.[0] = '1' then yield! extractNumbers (s.Substring 5) ]
        let numbers = extractNumbers (state.data.Substring 6)
        let offset = 6 + numbers.Length * 5
        { state with
            data = state.data.Substring(offset)
            versions = version::state.versions
            lengthType = decrease offset state.lengthType
        }
    else
        let l, lt =
            match state.data.[6] with
            | '0' -> 15, state.data.Substring(7, 15) |> bin2int |> Fixed
            | '1' -> 11, state.data.Substring(7, 11) |> bin2int |> Count
            | _   -> failwith "unexpected"
        { state with
            data = state.data.Substring(7 + l)
            versions = version::state.versions
            lengthType = [lt]
        }

let solve (input:string) =
    let rec loop state =
        if state.data = "" then state else
        loop (step state)
    let result = loop (parse input)
    result.versions
    |> List.sumBy bin2int


