module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

[<Tests>]
let all =
    testList "all" [
        testList "part 1" [
            test "single statement" { solve "forward 5" =! 0 }
            test "two statements" { solve "forward 5\r\ndown 5" =! 25 }
            test "parse down" { parseCommand "down 5" =! Down 5 }
            test "parse up" { parseCommand "up 5" =! Up 5 }
            test "create ship" { Ship1.create =! (0,0) }
            test "demoinput" { solve demoinput =! 150 }
            test "exec up" { Ship1.exec (0,2) (Up 2) =! (0,0) }
            test "exec down" { Ship1.exec (0,2) (Down 1) =! (0,3) }
            test "exec forward" { Ship1.exec (0,2) (Forward 1) =! (1,2) }
        ]
        testList "part 2" [
            test "create ship" {
                let s = Ship2.create
                s.dep =! 0
                s.pos =! 0
                s.aim =! 0 }
            test "exec up" {
                let result = Ship2.exec (Ship2.create) (Down 2)
                result.aim =! 2 }
            test "exec down" {
                let result = Ship2.exec (Ship2.create) (Up 2)
                result.aim =! -2 }
            test "exec forward" {
                let ship = { pos = 4; dep = 9; aim = -2 }
                let result = Ship2.exec ship (Forward 3)
                result.pos =! 7
                result.dep =! 3 }
            test "calc result" {
                let ship = { pos = 4; dep = 9; aim = -2 }
                Ship2.calcResult ship =! 36 }
            test "demoinput" { solve2 demoinput =! 900 }
        ]
    ]

