module Solution

open System

type Pixel = int * int

type Image = {
    enhancementRules : Map<int, int>
    pixels: Pixel Set
    isInverted : bool }

let charToBit = function
    | '#' -> 1
    | '.' -> 0

let parseRules =
    Seq.indexed >> Seq.map (fun (i,c) -> i, charToBit c) >> Map

let parsePixel y = function
    | x, '#' -> Some (x,y)
    | _      -> None

let parseLine (y, line) =
    line
    |> Seq.toList
    |> List.indexed
    |> List.choose (parsePixel y)

let parsePixels = List.indexed >> List.collect parseLine >> Set

let parseInput (input:string) =
    let lines = input.Split "\r\n" |> Array.toList
    {
        isInverted = false
        enhancementRules = parseRules lines.Head
        pixels = lines |> List.skip 2 |> parsePixels
    }

let boolToBit = function
    | false -> 0
    | true  -> 1

let boolsToInt =
    List.map (boolToBit >> string)
    >> String.concat ""
    >> fun s -> Convert.ToInt32(s, 2)

let squareAround (x,y) = [ for i in 0..8 do yield (x-1+i%3, y-1+i/3) ]

let containsPixel image p = image.isInverted <> image.pixels.Contains p

let enhancePoint image =
    squareAround
    >> List.map (containsPixel image)
    >> boolsToInt
    >> fun c -> image.enhancementRules.Item c = 1

let findMinMax xs = Seq.min xs, Seq.max xs

let boxAround pixels = [
    let xl,xh = pixels |> Seq.map fst |> findMinMax
    let yl,yh = pixels |> Seq.map snd |> findMinMax
    for y in yl-1..yh+1 do
    for x in xl-1..xh+1 do
        yield (x,y) ]

// If a square of empty pixels produces a new pixel invert the image.
// That means all pixels outside the viewed box are considered 'on'
let shouldInvert image =
    image.isInverted = false && image.enhancementRules.Item 0 = 1

let enhanceImage image =
    let inverted = shouldInvert image
    let newPixels =
        boxAround image.pixels
        |> List.filter (enhancePoint image >> (<>)inverted)
    { image with pixels = Set newPixels; isInverted = inverted }

let genericSolve n input =
    [1..n]
    |> Seq.fold (fun acc _ -> enhanceImage acc) (parseInput input)
    |> fun s -> s.pixels.Count

let solve  = genericSolve 2
let solve2 = genericSolve 50

