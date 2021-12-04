module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

let demoboard = "22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19"


[<Tests>]
let all =
    testList "all" [
        testList "part 1" [
            test "number lists" {
                let b = parseBoard demoboard
                b.history =! []
                b.lists |> List.contains (Set [22;13;17;11;0]) =! true
                b.lists |> List.contains (Set [1;12;20;15;19]) =! true

                b.lists |> List.contains (Set [22;8;21;6;1]) =! true
                b.lists |> List.contains (Set [0;24;7;5;19]) =! true }
            test "addNumber" {
                let b = parseBoard demoboard |> addNumber 22
                b.history =! [22]
                b.lists |> List.contains (Set [13;17;11;0]) =! true
                b.lists |> List.contains (Set [8;21;6;1]) =! true }
            test "bingo" {
                let b = {
                            history = []
                            lists = [Set[3];Set[5]]
                            bingo = false }
                        |> addNumber 5
                b.bingo =! true }
            test "no bingo" {
                let b = {
                            history = []
                            lists = [Set[3];Set[5;4]]
                            bingo = false }
                        |> addNumber 5
                b.bingo =! false } 
            test "parse input" {
                let hd, bs = parse @"1,2,3

1 2 3 4 5
6 7 8 9 10
10 11 12 13 14
15 16 17 18 20
20 21 22 23 24

1 2 3 4 5
6 7 8 9 10
10 11 12 13 14
15 16 17 18 20
20 21 22 23 25
"
                hd =! [1;2;3]
                bs.Length =! 2
                bs.[0].lists |> List.contains (Set [5;10;14;20;24]) =! true
                bs.[1].lists |> List.contains (Set [5;10;14;20;25]) =! true
            }
            test "demoinput" {
                solve demoinput =! 4512
            }
        ]
    ]


