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
                game2.pawns.Item 0 =! 6
                game2.scores.Item 0 =! 6
                game2.currentPlayer =! 1
                game2.turnCount =! 1
            }
            test "move with wrapping around" {
                let game =
                    { newGame with
                        pawns = Map [0,9; 1,8]
                        scores = Map [0,100; 1,200]
                        currentPlayer = 1
                        turnCount = 99
                    }
                let game2 = move 5 game
                game2.pawns.Item 1 =! 3
                game2.scores.Item 1 =! 203
                game2.currentPlayer =! 0
                game2.turnCount = 100
            }
            test "run game" {
                let game =
                    { newGame with
                        pawns = Map [0,4; 1,8]
                    }
                let game2 = runTo 1000 game
                game2.scores =! Map [0,1000; 1,745]
                game2.currentPlayer =! 1
                game2.turnCount =! 331
            }
            test "solve demoinput" {
                solve 4 8 =! 739785
            }
        ]
    ]


