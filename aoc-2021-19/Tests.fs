module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

[<Tests>]
let all =
    testList "all" [
        testList "orientation" [
            test "rotate vector" {
                let v = [1;2;3]
                let m = [
                    [1;0;0]
                    [0;1;0]
                    [0;0;1]]
                apply m v =! [1;2;3]
            }
            test "rotate vectors" {
                let vs = [
                   [-1;-1;1]
                   [-2;-2;2]
                   [-3;-3;3]
                   [-2;-3;1]
                   [5;6;-4]
                   [8;0;7 ] ]
                let m = [
                    [-1;0;0]
                    [0;0;-1]
                    [0;-1;0]]
                let vs2 = rotateVectors vs m
                vs2.[0] =! [1;-1;1]
                vs2.[5] =! [-8;-7;0]
                vs2 =! [
                    [ 1;-1;1 ]
                    [ 2;-2;2 ]
                    [ 3;-3;3 ]
                    [ 2;-1;3 ]
                    [ -5;4;-6 ]
                    [ -8;-7;0 ] ]
            }
        ]
        testList "parsing" [
            test "parse demoinput" {
                let scanners = parse demoinput
                scanners.Length =! 5
                scanners.[0].Length =! 25
                scanners.[0].[0] =! [404;-588;-901]
                scanners.[4].Length =! 26
                scanners.[4].[25] =! [30;-46;-14]
            }
        ]
        testList "overlapping" [
            test "overlap points" {
                let ps1 = [[0;2;0];[4;1;0];[3;3;0]]
                let ps2 = [[-1;-1;0];[-5;0;0];[-2;1;0]]
                let result = overlap ps1 ps2
                result.Length =! 3
            }
            test "demoinput" {
                let scanners = parse demoinput
                let o = overlapScanners scanners.[0] scanners.[1]
                o.Count =! 12
            }
        ]
    ]


