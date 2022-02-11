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
            test "compact state" {
                compact demoinput =! ".|.|BA|.|CD|.|BC|.|DA|.|."
            }
            test "expand state" {
                expand ".|.|BA|.|CD|.|BC|.|DA|.|." =! demoinput
            }
            //test "pod at 1 (corridor) can move 1 to the left" {
            //    let result = "|A" |> allLeftMovesFor 1 |> Set
            //    result.Contains ( "A|", 1 ) =! true
            //}
            //test "pod at 2 (sideroom) can move 1-2 to the left" {
            //    let result = "||A" |> allLeftMovesFor 2 |> Set
            //    result.Contains ( "A||", 3 ) =! true
            //    result.Contains ( "|A|", 2 ) =! true
            //}
            //test "pod cant move to left if corridor blocked" {
            //    let result = "B|A" |> allLeftMovesFor 1 |> Set
            //    result.Count =! 0
            //    let result = "B||A" |> allLeftMovesFor 2 |> Set
            //    result.Count =! 1
            //}
            //test "pod can move left to/over sideroom if empty" {
            //    let result = "|||A" |> allLeftMovesFor 3 |> Set
            //    result.Contains ( "||A|", 2 ) =! true
            //    result.Contains ( "|A||", 2 ) =! true
            //    result.Contains ( "A|||", 3 ) =! true
            //    result.Count =! 3
            //}
            //test "pod can move left to target sideroom" {
            //    let result = "|||A" |> allLeftMovesFor 3 |> Set
            //    result.Contains ( "||A|", 2 ) =! true
            //}
            //test "pod cant move left to wrong sideroom" {
            //    let result = "|||B" |> allLeftMovesFor 3 |> Map
            //    result.ContainsKey "||B|" =! false
            //}
            //test "pod can move right to/over sideroom if empty" {
            //    let result = "||B|||" |> allRightMovesFor 2 |> Set
            //    result.Contains ( "|||B||", 20 ) =! true
            //    result.Contains ( "||||B|", 40 ) =! true
            //    result.Contains ( "|||||B", 40 ) =! true
            //    result.Count =! 3
            //}
            //test "no left moves if no pod" {
            //    let result = "A||C" |> allLeftMovesFor 1 |> Set
            //    result.Count =! 0
            //}
            //test "example step 1 is found" {
            //    let xs = options     "||BA||CD||BC||DA||" |> Set
            //    Expect.contains xs ( "||BA|B|CD||C||DA||", 40 ) ""
            //}
            //test "example step 2 is found" {
            //    let xs = options     "||BA|B|CD||C||DA||" |> Set
            //    Expect.contains xs ( "||BA|B|D||CC||DA||", 400 ) ""
            //}
            //test "example step 3 is found" {
            //    let xs = options     "||BA|B|D||CC||DA||" |> Set
            //    Expect.contains xs ( "||BA|B||D|CC||DA||", 3000 ) ""
            //    // fails because D needs to take two steps out of the sideroom : bad model!
            //}
        ]
    ]


