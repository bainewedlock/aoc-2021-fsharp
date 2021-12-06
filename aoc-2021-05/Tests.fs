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
            test "parse" { parseLine "0,4 -> 5,-3" =! (0,4,5,-3) }
            test "pixels to right" {
                pixels (0,0,1,0) =! Set [
                    0,0
                    1,0 ] }
            test "pixels to left" {
                pixels (9,7,7,7) =! Set [
                    9,7
                    8,7
                    7,7 ] }
            test "pixels down" {
                pixels (1,1,1,3) =! Set [
                    1,1
                    1,2
                    1,3 ] }
            test "pixels up" {
                pixels (0,0,0,-1) =! Set [
                    0,0
                    0,-1 ] }
        ]
        testList "part 2" [
            test "pixels down-right" {
                pixels (1,6,3,4) =! Set [
                    1,6
                    2,5
                    3,4 ] } ]
    ]


