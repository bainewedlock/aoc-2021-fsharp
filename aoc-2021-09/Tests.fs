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
            test "parse demoinput" {
                let grid = parse demoinput
                grid.Item (0,0) =! 2
                grid.Item (1,2) =! 8 }
            test "adjacent" {
                adjacent (0,0) =! [
                    1,0
                    -1,0
                    0,1
                    0,-1
                ]
            }
            test "lowpoints" {
                let result = lowpoints (parse demoinput) 
                result.Length =! 4
                result |> List.contains ((2,2), 5) =! true
            }
            test "solve demoinput" {
                solve demoinput =! 15
            }
        ]
        testList "part 2" [
            test "get non-9 positions" {
                let g  = parse demoinput
                let g2 = getNon9Positions g
                g2.Count =! g.Count - 15
            }
            test "extract basin" {
                let g = demoinput |> parse |> getNon9Positions
                let b, g2 = extractBasin g
                b |> Set.count =! 3
                g2.Count =! g.Count - 3
            }
            test "solve demoinput" {
                solve2 demoinput =! 1134
            }
        ]
    ]


