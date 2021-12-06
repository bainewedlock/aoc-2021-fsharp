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
            test "parse" {
                parse "3,4,3,1,2" |> Map =! Map [
                    3, 2UL
                    4, 1UL
                    1, 1UL
                    2, 1UL ] }
            test "step" {
                step [1,2UL] =! [0,2UL]
                step [0,2UL] =! [6,2UL;8,2UL] }
            test "demoinput" {
                solve "3,4,3,1,2" =! "5934" }
            test "reduce" {
                reduce [1,2UL;1,5UL;2,1UL] =! [1,7UL;2,1UL] }
        ]
        testList "part 2" [
            test "solve" {
                solve2 "3,4,3,1,2" =! "26984457539" }
        ]
    ]


