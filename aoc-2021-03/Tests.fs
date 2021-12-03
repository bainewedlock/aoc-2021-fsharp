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
            test "extract" {
                extract Seq.maxBy 0 ["ax";"by";"az"] =! 'a'
            }
            test "demoinput" {
                demoinput |> parse |> extractAll Seq.maxBy =! "10110"
                solve demoinput =! 198 }
        ]
        testList "part 2" [
            test "keepAt" {
                keepAt Seq.maxBy 0 ["ax";"by";"az"] =! ["ax"; "az"] }
            test "demoinput" {
                demoinput |> parse |> find Seq.maxBy =! "10111"
                solve2 demoinput =! 230 }
        ]
    ]


