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
                let grid = parse demoinput
                grid.cells.Item (0,0) =! 1
                grid.cells.Item (0,2) =! 2
                grid.cells.Item (9,9) =! 1
                grid.cells.Item (9,7) =! 9
                grid.pos =! (0,0)
                grid.visited =! Set [0,0]
                grid.risk =! 0
                grid.goal =! (9,9)
            }
            test " options" {
                let grid =
                    { parse demoinput
                        with pos = (0,1); visited = Set [0,0; 0,1] }
                let opts = options grid
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
            test "duplicateInput" {
                let grid = demoinput |> parse |> duplicate
                grid.cells.Count =! 100 * 25
            }
        ]
    ]


