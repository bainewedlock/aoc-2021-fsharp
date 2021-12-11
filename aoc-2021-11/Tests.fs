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
                let grid = parse @"11111
                                   19991
                                   19181
                                   19991
                                   11112"
                grid.Item (0,1) =! 1
                grid.Item (3,2) =! 8
                grid.Item (4,4) =! 2
            }
            test "increase single if not 9" {
                let grid = parse @"82"
                let grid2 = increaseAt (0,0) grid
                grid2.Item (0,0) =! 9
                grid2.Item (1,0) =! 2
            }
            test "flash if 9" {
                let grid = parse @"923"
                let grid2 = increaseAt (0,0) grid
                grid2.Item (0,0) =! 10
                grid2.Item (1,0) =! 3
            }
            test "flash chain" {
                let grid = parse @"989"
                let grid2 = increaseAt (0,0) grid
                grid2.Item (0,0) =! 10
                grid2.Item (1,0) =! 9
                grid2.Item (2,0) =! 9
                let grid3 = increaseAt (1,0) grid2
                grid3.Item (0,0) =! 11
                grid3.Item (1,0) =! 11
                grid3.Item (2,0) =! 10
            }
            test "increaseEnergy" {
                let grid = parse @"91"
                let grid2 = increaseEnergy grid
                grid2.Item (1,0) =! 3
                grid2.Item (0,0) =! 10
            }
            test "demo step 1" {
                let grid = parse @"11111
                                   19991
                                   19191
                                   19991
                                   11111"
                let flashes, grid2 = step grid
                grid2.Item (1,1) =! 0
                grid2.Item (2,2) =! 0
                grid2.Item (3,3) =! 0
                flashes =! 9
            }
            test "steps" {
                let grid = parse demoinput
                let flashes, _ = steps 2 grid
                flashes =! 35
                let flashes, grid3 = steps 3 grid
                flashes =! 35 + 45 
                grid3 =!    parse @"0050900866
                                    8500800575
                                    9900000039
                                    9700000041
                                    9935080063
                                    7712300000
                                    7911250009
                                    2211130000
                                    0421125000
                                    0021119000"
                let flashes, _ = steps 10 grid
                flashes =! 204
            }
        ]
        testList "part 2" [
            test "synchronize" {
                parse demoinput
                |> synchronize 0 =! 195
            }
        ]
    ]


