module Solution

type Command = Down of int | Up of int | Forward of int

let parseCommand (line:string) =
    match line.Split " " with
    | [|"forward"; x|] -> Forward (int x)
    | [|"down";    x|] -> Down (int x)
    | [|"up";      x|] -> Up (int x)
    | _ -> failwithf "unexpected line: %s" line

let parseInput (x:string) = x.Split "\r\n" |> Seq.map parseCommand

module Ship1 =
    let create = (0,0)
    let calcResult (a,b) = a*b
    let exec (x,y) cmd =
        match cmd with
        | Forward dx -> x+dx, y
        | Up   dy    -> x, y-dy
        | Down dy    -> x, y+dy

type Ship2 = { pos : int; dep : int; aim : int }

module Ship2 =
    let create = { pos = 0; dep = 0; aim = 0 }
    let calcResult ship = ship.pos * ship.dep
    let exec ship = function
    | Down x    -> { ship with aim = ship.aim + x }
    | Up   x    -> { ship with aim = ship.aim - x }
    | Forward x -> { ship with pos = ship.pos + x
                               dep = ship.dep + x*ship.aim }

let solve =
    parseInput
    >> Seq.fold Ship1.exec Ship1.create
    >> Ship1.calcResult

let solve2 =
    parseInput
    >> Seq.fold Ship2.exec Ship2.create
    >> Ship2.calcResult

