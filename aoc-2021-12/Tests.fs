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
                let map = demoinput |> parse |> setupMap
                map.currentCave =! StartCave
                map.path =! [StartCave]
                map.remainingCaves.Count =! 5
                map.links.Contains (SmallCave "b", SmallCave "d") =! true
                map.links.Contains (SmallCave "d", SmallCave "b") =! true
            }
            test "options" {
                let map =
                    {
                        currentCave = StartCave
                        path = [StartCave]
                        remainingCaves = Set [ EndCave; SmallCave "a" ]
                        links = Set [StartCave,EndCave]
                        visitCount =  Map [EndCave, 0]
                    }
                let os = options 1 map
                os.Length =! 1
                os.[0].currentCave =! EndCave
                os.[0].path =! [EndCave; StartCave]
                os.[0].remainingCaves =! Set [ SmallCave "a" ]
            }
            test "small cave only once" {
                let map =
                    {
                        currentCave = StartCave
                        path = [StartCave]
                        remainingCaves = Set [ SmallCave "a" ]
                        links = Set [StartCave,SmallCave "a"]
                        visitCount = Map [SmallCave "a", 0]
                    }
                let os = options 1 map
                os.Length =! 1
                os.[0].currentCave =! SmallCave "a"
                os.[0].path =! [SmallCave "a"; StartCave]
                //os.[0].remainingCaves =! Set []
            }
            test "big cave repeatedly" {
                let map =
                    {
                        currentCave = StartCave
                        path = [StartCave]
                        remainingCaves = Set [ BigCave "A" ]
                        links = Set [StartCave,BigCave "A"]
                        visitCount =  Map [BigCave "A", 0]
                    }
                let os = options 1 map
                os.Length =! 1
                os.[0].currentCave =! BigCave "A"
                os.[0].path =! [BigCave "A"; StartCave]
                os.[0].remainingCaves =! Set [BigCave "A"]
            }
            test "stop at endcave" {
                let map =
                    {
                        currentCave = EndCave
                        path = [EndCave]
                        remainingCaves = Set [ BigCave "A" ]
                        links = Set [EndCave,BigCave "A"]
                        visitCount = Map.empty
                    }
                let os = options 1 map
                os.Length =! 0
            }
            test "paths" {
                let ps = demoinput |> parse |> paths 1 |> Seq.toList
                ps.Length =! 10 
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
            test "setup map" {
                let map = demoinput |> parse |> setupMap
                map.visitCount.Item (SmallCave "b") =! 0
            }
            test "demoinput" {
                let foo =debug demoinput
                solve2 demoinput =! 36
                solve2 "start-A
                        start-b
                        A-c
                        A-b
                        b-d
                        A-end
                        b-end" =! 36 // nur 1 small cave doppelt!
            }
            test "no more then one small cave repeats" {
                let map =
                    {
                        currentCave = StartCave
                        path = [StartCave]
                        remainingCaves = Set [ SmallCave "a" ]
                        links = Set [ StartCave, SmallCave "a" ]
                        visitCount =  Map [ SmallCave "a", 0
                                            SmallCave "c", 2]
                    }

                let os = options 2 map
                os.Length =! 1
                os.[0].currentCave =! SmallCave "a"
                os.[0].path =! [SmallCave "a"; StartCave]
            }
        ]
    ]


