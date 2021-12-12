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
            test "parsing" {
                let links = parse demoinput
                links.[0] =! (StartCave, BigCave "A")
                links.[6] =! (SmallCave "b", EndCave)
            }
            test "setup map" {
                let map = demoinput |> parse |> setupMap 1
                map.currentCave =! StartCave
                map.path =! [StartCave]
                map.links.Contains (SmallCave "b", SmallCave "d") =! true
                map.links.Contains (SmallCave "d", SmallCave "b") =! true
                map.remaining.Item (SmallCave "b") =! 1
            }
            test "options" {
                let map =
                    { defaultMap with
                        links = Set [StartCave,EndCave]
                        remaining = Map [ EndCave, 1 
                                          SmallCave "a", 1]
                    }
                let os = options map
                os.Length =! 1
                os.[0].currentCave =! EndCave
                os.[0].path =! [EndCave; StartCave]
                os.[0].remaining.TryFind (EndCave) =! None
                os.[0].remaining.Item (SmallCave "a") =! 1
            }
            test "small cave only once" {
                let map =
                    { defaultMap with
                        links = Set [StartCave,SmallCave "a"]
                        remaining = Map [
                            SmallCave "a", 1 ]
                    }
                let os = options map
                os.Length =! 1
                os.[0].currentCave =! SmallCave "a"
                os.[0].path =! [SmallCave "a"; StartCave]
            }
            test "big cave repeatedly" {
                let map =
                    { defaultMap with
                        links = Set [StartCave,BigCave "A"]
                        remaining = Map [ BigCave"A", 1]
                    }
                let os = options map
                os.Length =! 1
                os.[0].currentCave =! BigCave "A"
                os.[0].path =! [BigCave "A"; StartCave]
                os.[0].remaining.Item (BigCave "A") =! 1
            }
            test "stop at endcave" {
                let map =
                    { defaultMap with
                        links = Set [EndCave,BigCave "A"]
                        remaining = Map.empty
                    }
                let os = options map
                os.Length =! 0
            }
            test "paths" {
                demoinput
                |> parse
                |> setupMap 1
                |> paths
                |> Seq.length =! 10
            }
            test "demo #2" {
                @"dc-end
                  HN-start
                  start-kj
                  dc-start
                  dc-HN
                  LN-dc
                  HN-end
                  kj-sa
                  kj-HN
                  kj-dc"
                |> solve =! 19
            }
        ]
        testList "part 2" [
            test "demoinput" {
                solve2 demoinput =! 36
                solve2 "start-A
                        start-b
                        A-c
                        A-b
                        b-d
                        A-end
                        b-end" =! 36
            }
            test "after small cave twice allow other only 1" {
                let map =
                    { defaultMap with
                        links = Set [StartCave,SmallCave "a"]
                        path = [SmallCave "a"]
                        remaining = Map [
                            SmallCave "a", 1
                            SmallCave "b", 2
                        ]
                    }
                let os = options map
                os.Length =! 1
                os.[0].currentCave =! SmallCave "a"
                os.[0].remaining.Item (SmallCave "b") =! 1
            }
        ]
    ]


