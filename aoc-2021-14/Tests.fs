module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

[<Tests>]
let all =
    testList "solution" [
        test "parsing" {
            let input = parseInput demoinput
            input.template =! "NNCB"
            input.rules.Count =! 16
        }
        test "step" {
            let counts = parseInput demoinput |> step 2 |> Map
            counts.Item "CB" =! 2UL
            counts.Item "BC" =! 2UL
            counts.Item "BB" =! 2UL
        }
        test "solve demoinput" {
            solve demoinput =! "1588"
        }
        test "solve2 demoinput" {
            solve2 demoinput =! "2188189693529"
        }
    ]


