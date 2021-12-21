module Solution

type Game = {
    pawns : Map<int, int>
    scores : Map<int, int>
    currentPlayer : int
    turnCount : int
}

let newGame = {
    pawns = Map [0,1; 1,1]
    scores = Map [0,0; 1,0]
    currentPlayer = 0
    turnCount = 0 }

let rec wrapAt n d x =
    if x < n then x else
    x-d
    |> wrapAt n d

let move d game =
    let p = game.currentPlayer
    let newPos = (game.pawns.Item p) + d |> wrapAt 11 10
    let newScore = (game.scores.Item p) + newPos
    { game with
        pawns = game.pawns.Add(p, newPos)
        scores = game.scores.Add(p, newScore)
        currentPlayer = p + 1 |> wrapAt 2 2
        turnCount = game.turnCount + 1
    }

let rec runTo score game =
    if game.scores |> Map.exists (fun _ s -> s >= score) then game else
    let dice = game.turnCount * 9 + 6 
    move dice game
    |> runTo score

let solve p1 p2 =
    let game =
        { newGame with pawns = Map [0,p1; 1,p2] }
        |> runTo 1000
    game.scores.Item(game.currentPlayer) * game.turnCount * 3
