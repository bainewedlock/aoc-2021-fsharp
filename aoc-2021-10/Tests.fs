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
            test "findError" {
                findError "[]" =! Valid
                findError "[)" =! Unexpected ')'
                findError "{([(<{}[<>[]}>{[]{[(<()> " =! Unexpected '}'
                findError "[[<[([]))<([[{}[[()]]]"    =! Unexpected ')'
                findError "[{[{({}]{}}([{[{{{}}([]"   =! Unexpected ']'
                findError "[<(<(<(<{}))><([]([]()"    =! Unexpected ')'
                findError "<{([([[(<>()){}]>(<<{{"    =! Unexpected '>'
            }
            test "score" {
                scorePart1 (Unexpected ')') =! 3
                scorePart1 (Unexpected ']') =! 57
                scorePart1 (Unexpected '}') =! 1197
                scorePart1 (Unexpected '>') =! 25137
            }
            test "solve demoinput" {
                solve demoinput =! 26397
            }
        ]
        testList "part 2" [
            test "findError" {
                findError "[({(<(())[]>[[{[]{<()<>>" =!
                    Incomplete "}}]])})]"
            }
            test "middle" {
                middle [1;2;3;4;5] =! 3
            }
            test "score missing characters" {
                scorePart2 "}}]])})]" =! 288957UL
                scorePart2 ")}>]})" =! 5566UL
                scorePart2 "}}>}>))))" =! 1480781UL
                scorePart2 "]]}}]}]}>" =! 995444UL
                scorePart2 "])}>" =! 294UL
            }
            test "demoinput" {
                solve2 demoinput =! 288957UL
            }
        ]
    ]


