module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

[<Tests>]
let all =
    testList "all" [
        testList "moving pawns" [
            test "move without wrapping around" {
                let game = newGame
                let game2 = move 5 game
                game2.pawns =! (6, 1)
                game2.scores =! (6, 0)
                game2.turns =! 1
            }
            test "move with wrapping around" {
                let game =
                    { newGame with
                        pawns = (8, 9)
                        scores = (200, 100)
                        turns = 99 }
                let game2 = move 5 game
                game2.pawns =! (3, 9)
                game2.scores =! (203, 100)
                game2.turns =! 100
            }
            test "nextPlayer" {
                let game =
                    { newGame with
                        pawns = 1,2
                        scores = 3,4 }
                    |> nextPlayer
                game.pawns =! (2,1)
                game.scores =! (4,3)
                
            }
            test "run game" {
                let game = { newGame with pawns = 4,8 }
                let game2 = runTo 1000 game
                game2.scores =! (745,1000)
                game2.turns =! 331
            }
            test "solve demoinput" {
                solve 4 8 =! 739785
            }
            test "solve2 demoinput" {
                solve2 4 8 =! "444356092776315"
            }
        ]
    ]


