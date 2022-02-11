module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

[<Tests>]
let all =
    testList "all" [
        testList "model" [
            test "parsing" {
                let sea = parse demoinput
                Expect.equal (sea.GetLength 0) 10 "wrong width"
                Expect.equal (sea.GetLength 1) 9 "wrong height"
                sea.[0,0] =! 'v'
                sea.[1,0] =! '.'
                sea.[9,8] =! '>'
            }
            test "printing" {
                demoinput |> parse |> print =! demoinput
            }
        ]
        testList "movement" [
            test "move to the right" {
                let sea = parse ">.."
                tick sea
                print sea =! ".>."
                tick sea
                print sea =! "..>"
                tick sea
                print sea =! ">.."
            }
            test "move down" {
                let sea = parse "v
                                 ."
                tick sea
                sea =! parse ".
                              v"
            }
            test "step 1" {
                let sea = parse demoinput
                tick sea
                print sea =!  "....>.>v.>
v.v>.>v.v.
>v>>..>v..
>>v>v>.>.v
.>v.v...v.
v>>.>vvv..
..v...>>..
vv...>>vv.
>.v.v..v.v"
            }
        ]
        testList "solve" [
            test "part 1" { solve demoinput =! 58 }
        ]
    ]


