module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

[<Tests>]
let all =
    testList "all" [
        testList "foo" [
            test "bar" {
                1 =! 1
            }
        ]
    ]


