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
                let state = parse "D2FE28"
                state.data =! "110100101111111000101000"
                state.lengthType =! [Count 1]
                state.versions =! []
            }
            test "step over a literal" {
                let state =
                    {
                        data = D "110.100.10111.11110.00101.000"
                        lengthType = [Count 1]
                        versions = []
                    }
                let state2 = step state
                state2.versions =! ["110"]
                state2.lengthType =! []
                state2.data =! "000"
            }
            test "step over a operation with 1 literal element" {
                let state =
                    {
                        data = D "001.110.0.000000000011011.110.100.01010.010.100.10001.00100.0000000"
                        lengthType = [Count 1]
                        versions = []
                    }
                let state2 = step state
                state2.versions =! ["001"]
                state2.lengthType =! [Fixed 27]
                state2.data =! D "110.100.01010.010.100.10001.00100.0000000"
            }
            test "step over the first element of an operation (Count)" {
                let state =
                    {
                        data = D "010.100.00001.100.100.00010.001.100.00011.00000"
                        lengthType = [Count 3]
                        versions = []
                    }
                let state2 = step state
                state2.versions =! ["010"]
                state2.lengthType =! [Count 2]
                state2.data =! D "100.100.00010.001.100.00011.00000"
            }
            test "step over the first element of an operation (Fixed)" {
                let state =
                    {
                        data = D "110.100.01010.010.100.10001.00100.0000000"
                        lengthType = [Fixed 27]
                        versions = []
                    }
                let state2 = step state
                state2.versions =! ["110"]
                state2.lengthType =! [Fixed 16]
                state2.data =! D "010.100.10001.00100.0000000"
            }
            test "step out after the last element of an operation (Fixed)" {
                let state =
                    {
                        data = D "010.100.10001.00100.0000000"
                        lengthType = [Fixed 16]
                        versions = []
                    }
                let state2 = step state
                state2.versions =! ["010"]
                state2.lengthType =! []
                state2.data =! D "0000000"
            }
            test "another packet on top level" {
                let state =
                    {
                        data = D "010.100.00001.010.100.00001"
                        lengthType = []
                        versions = []
                    }
                let state2 = step state
                state2.versions =! ["010"]
                state2.lengthType =! []
                state2.data =! D "010.100.00001"
            }
            test "trailing zeros" {
                let state =
                    {
                        data = D "0000"
                        lengthType = []
                        versions = []
                    }
                let state2 = step state
                state2.versions =! []
                state2.lengthType =! []
                state2.data =! D ""
            }
            test "solve example #1" {
                solve "8A004A801A8002F478" =! 16
            }
            test "solve example #3" {
                solve "A0016C880162017C3686B18A3D4780" =! 31
            }
        ]
    ]


