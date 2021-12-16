module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

let D (s:string) = s.Replace(".", "")

[<Tests>]
let all =
    testList "all" [
        testList "part 1" [
            test "parsing" {
                parse "D2FE28" =! "110100101111111000101000"
            }
            test "describe 1 literal" {
                describe "110100101111111000101000"
                    =! [
                        Version 6
                        Literal 2021UL
                    ]
            }
            test "describe 1 operator" {
                describe "00111000000000000110111101000101001010010001001000000000"
                    =! [
                        Version 1
                        OperatorNo 6
                        Version 6
                        Literal 10UL
                        Version 2
                        Literal 20UL
                    ]
            }
        ]
    ]


