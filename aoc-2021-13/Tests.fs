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
            test "parsing" {
                let instructions = parse demoinput
                instructions.points.Contains (6,10) =! true
                instructions.points.Contains (9,0)  =! true
                instructions.folds =! [FoldY 7; FoldX 5]
            }
            test "foldY" {
                let points = foldPoints (FoldY 1) (Set [0,0; 1,2])
                points =! Set [0,0; 1,0]
            }
            test "foldX" {
                let points = foldPoints (FoldX 1) (Set [0,0; 2,2])
                points =! Set [0,0; 0,2]
            }
            test "solve demoinput" {
                solve demoinput =! 17
            }

        ]
    ]


