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
            test "multiplyGrid" {
                let grid = demoinput |> parse |> multiplyGrid
                grid.cells.Count =! 100 * 25
                grid.cells.Item (0,0) =! 1
                grid.cells.Item (47,49) =! 4
                grid.cells.Item (48,49) =! 7
                grid.cells.Item (49,49) =! 9
                grid.goal =! (49,49)
            }
            test "solve demoinput" {
                solve2 demoinput =! 315
            }
        ]
        testList "priority queue" [
            test "heappush" {
                heappush 3 [1;2;4] =! [1;2;3;4]
                heappush (1,5) [(0,3);(5,5)] =! [(0,3);(1,5);(5,5)]
            }
        ]
    ]


