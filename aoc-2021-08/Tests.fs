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
            test "parse" {
                let result = parse "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
                result.signals =! ["be";"cfbegad";"cbdgef";"fgaecd";"cgeb";"fdcge";"agebfd";"fecdb";"fabcd";"edb"]
                result.outputs =! ["fdgacbe";"cefdb";"cefbgd";"gcbe"] }
            test "eval" {
                eval ["aaaa"; "a" ] =! 1 }
            test "solve demoinput" {
                solve demoinput =! 26 }
        ]
        testList "part 2" [
            test "decode" {
                "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
                |> parse
                |> decode 
                    =! 5353
            }
            test "decode simple" {
                let s = decodeSimple [ "12"; "123"; "1234"; "12345"; "1234567" ]
                s.decoded =! Map [
                    1, "12"
                    7, "123"
                    4, "1234"
                    8, "1234567" ]
                s.todo =! ["12345"] }
            test "findCode" {
                let s = {
                    decoded = Map [ 7, "abd" ]
                    todo    = [ "abcdf"; "bcdef" ]
                }
                findCode s 5 7 3 =! "abcdf"
            }
            test "countMatchingChars" {
                countMatchingChars  "abc" "cax" =! 2 }
            test "decodeComplex" {
                let s = { 
                    decoded = Map [7, "abd"; 4, "abef"]
                    todo = [
                        "abcdf"
                        "bcdef"
                        "acdfg"
                        "bcdefg"
                        "abcdef"
                        "abcdeg"]
                }
                let s2 = decodeComplex s
                s2.decoded.Item 3 =! "abcdf"
                s2.decoded.Item 5 =! "bcdef"
                s2.decoded.Item 2 =! "acdfg"
                s2.decoded.Item 6 =! "bcdefg"
                s2.decoded.Item 9 =! "abcdef"
                s2.decoded.Item 0 =! "abcdeg"
            }
            test "sortLetters" {
                sortLetters "acbde" =! "abcde" }
            test "solve2 demoinput" {
                solve2 demoinput =! 61229 }
        ]
    ]


