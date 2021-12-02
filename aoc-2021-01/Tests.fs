module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

[<Tests>]
let all =
    testList "all" [
        testList "part 1" [ test "demoinput" { solve demoinput =! 7 } ]
        testList "part 2" [ test "demoinput" { solve2 demoinput =! 5 } ]
    ]


