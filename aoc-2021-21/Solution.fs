module Solution

type Game = {
    pawns : int * int
    scores : int * int
    turns : int
}

let newGame = {
    pawns = 1, 1
    scores = 0, 0
    turns = 0 }

let movePawn d (a,b) =
    let rec loop x =
        if x <= 10 then x else
        loop (x-10)
    loop (a+d), b

let move d game =
    let addScore x (a,b) = (a+x, b)
    let newPos = movePawn d game.pawns
    { game with
        turns = game.turns + 1
        pawns = newPos
        scores = addScore (fst newPos) game.scores }

let nextPlayer game =
    let swap (a,b) = b,a
    { game with
        pawns  = swap game.pawns
        scores = swap game.scores }

let rec runTo score game =
    if snd game.scores >= score then game else
    let dice = game.turns * 9 + 6 
    move dice game
    |> nextPlayer
    |> runTo score

let solve p1 p2 =
    let game = { newGame with pawns = p1, p2 } |> runTo 1000
    fst game.scores * game.turns * 3
