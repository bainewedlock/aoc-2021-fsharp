module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

let dump o = sprintf "%A" o

[<Tests>]
let all =
    testList "all" [
        testList "part 1" [
            test "example" {
                let input = parseInput demoinput
                input =! "||BA|CD|BC|DA||"
            }
            test "options" {
                let result = options "||BA||CD||BC||DA||" |> Map
                result.Item "B||A||CD||BC||DA||" =! 30
                result.Item "|B|A||CD||BC||DA||" =! 20
                result.Item "D||BA||CD||BC||A||" =! 9000
                result.Item "||BA||CD||BC||A||D" =! 3000
                result.Item "||BA||D||BC||DA|C|" =! 600
            }
            test "parse state" {
                parseState "||BA||CD" =! [[];[];['B';'A'];[];['C';'D']]
            }
            test "print state" {
                printState [[];[];['B';'A'];[];['C';'D']] =! "||BA||CD"
            }
            test "pod at 1 (corridor) can move 1 to the left" {
                let result = "|A" |> allLeftMovesFor 1 |> Set
                result.Contains ( "A|", 1 ) =! true
            }
            test "pod at 2 (sideroom) can move 1-2 to the left" {
                let result = "||A" |> allLeftMovesFor 2 |> Set
                result.Contains ( "A||", 3 ) =! true
                result.Contains ( "|A|", 2 ) =! true
            }
            test "pod cant move to left if corridor blocked" {
                let result = "B|A" |> allLeftMovesFor 1 |> Set
                result.Count =! 0
                let result = "B||A" |> allLeftMovesFor 2 |> Set
                result.Count =! 1
            }
            test "pod can move left to/over sideroom if empty" {
                let result = "|||A" |> allLeftMovesFor 3 |> Set
                result.Contains ( "||A|", 2 ) =! true
                result.Contains ( "|A||", 2 ) =! true
                result.Contains ( "A|||", 3 ) =! true
                result.Count =! 3
            }
            test "pod can move left to sideroom if same content" {
                let result = "||A|A" |> allLeftMovesFor 3 |> Set
                result.Contains ( "||AA|", 2 ) =! true
            }
            test "pod cant move left to sideroom if other content" {
                let result = "||B|A" |> allLeftMovesFor 3 |> Set
                result.Contains ( "||AA|", 2 ) =! false
            }
            test "pod can move right to/over sideroom if empty" {
                let result = "B|||" |> allRightMovesFor 0 |> Set
                result.Contains ( "|B||", 10 ) =! true
                result.Contains ( "||B|", 30 ) =! true
                result.Contains ( "|||B", 30 ) =! true
                result.Count =! 3
            }
            test "no left moves if no pod" {
                let result = "A||C" |> allLeftMovesFor 1 |> Set
                result.Count =! 0
            }
        ]
    ]


