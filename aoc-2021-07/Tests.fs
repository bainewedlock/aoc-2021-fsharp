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
            test "examples" {
                solve "1" =! 0
                solve "2" =! 0
                solve "3,3" =! 0
                solve "3,4,5" =! 2
            }
            test "solve" {
                solve demoinput =! 37 }
        ]
        testList "part 2" [
            test "fuel" {
                sumOfRange 0 =! 0
                sumOfRange 1 =! 1
                sumOfRange 2 =! 3
                sumOfRange 3 =! 6
                sumOfRange 11 =! 66
            }
            test "solve" {
                solve2 demoinput =! 168 }
        ]
    ]


