module Solution

open System

type Point = int * int

type Image = {
    bits : Map<int, int>
    cells: Point Set
    isInverted : bool
}

let charToBit = function
    | '#' -> 1
    | '.' -> 0

[<RequireQualifiedAccess>]
module Image =
    let countCells input =
        if input.isInverted then failwith "counting cells on inverted image"
        input.cells.Count
    let containsPixel p input =
        if input.isInverted then
            if input.cells.Contains p then false
            else true
        else
            if input.cells.Contains p then true
            else false

let parse (input:string) =
    let lines = input.Split "\r\n" |> Array.toList
    {
        isInverted = false
        bits =
            lines.[0]
            |> Seq.indexed
            |> Seq.map (fun (i,c) ->
                i, charToBit c)
            |> Map
        cells =
            lines
            |> List.skip 2
            |> List.indexed
            |> List.collect (fun (y,line) -> 
                line
                |> Seq.toList
                |> List.indexed
                |> List.choose (fun (x, c) ->
                    if (c = '#') then Some (x,y) else None))
            |> Set
    }

let boolToBit = function
    | false -> 0
    | true  -> 1

let enhancePoint (x0,y0) input =
    let bit =
        [ for i in 0..8 do yield (x0-1+i%3, y0-1+i/3) ]
        |> List.map (fun p -> Image.containsPixel p input)
        |> List.map (boolToBit >> string)
        |> String.concat ""
        |> fun s -> Convert.ToInt32(s, 2)
    input.bits.Item bit


let findMinMax : int seq -> int * int =
    Seq.fold (fun (lo,hi) x -> (min x lo, max x hi)) (+99999, -99999)

let enhanceImage input =
    let (xl,xh) = findMinMax (Set.map fst input.cells)
    let (yl,yh) = findMinMax (Set.map snd input.cells)

    let invert, check =
        match input.isInverted, input.bits.Item 0, input.bits.Item 511 with
        | false, 1, _ -> true, 0
        | true,  _, 0 -> false, 1
        | _        -> false, 1

    let newCells = [
        for y in yl-1..yh+1 do
        for x in xl-1..xh+1 do
            if enhancePoint (x,y) input = check then yield (x,y) ]


    { input with cells = Set newCells
                 isInverted = invert }

let solve input =
    input
    |> parse
    |> enhanceImage
    |> enhanceImage
    |> fun s -> s.cells.Count

