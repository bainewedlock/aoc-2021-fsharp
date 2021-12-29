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
                let input = parse demoinput
                input.cells.Item (0,0) =! 1
                input.cells.Item (0,2) =! 2
                input.cells.Item (9,9) =! 1
                input.cells.Item (9,7) =! 9
                input.pos =! (0,0)
                input.goal =! (9,9)
            }
            test "neighbours" {
                let grid = { parse demoinput with pos = (0,1)}
                let opts = neighbours (0,1) grid.cells
                opts.Length =! 3
                opts.[0] =! (1,1)
                opts.[1] =! (0,2)
                opts.[2] =! (0,0)
            }
            test "solve demoinput" {
                solve demoinput =! 40
            }
        ]
        testList "part 2" [
            test "multiplyGrid" {
                let input = demoinput |> parse |> multiplyGrid
                input.cells.Count =! 100 * 25
                input.cells.Item (0,0) =! 1
                input.cells.Item (47,49) =! 4
                input.cells.Item (48,49) =! 7
                input.cells.Item (49,49) =! 9
                input.goal =! (49,49)
            }
            test "solve demoinput" {
                solve2 demoinput =! 315
            }
        ]
    ]


