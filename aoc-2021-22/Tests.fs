module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

[<Tests>]
let all =
    testList "all" [
        testList "parsing" [
            test "a line" {
                let c = parse "on x=-48..6,y=-13..40,z=-12..35"
                c =! (On, {  
                        x0 = -48
                        x1 =   6
                        y0 = -13
                        y1 =  40
                        z0 = -12
                        z1 =  35 })
            }
            test "a line with switched coordinates" {
                let c = parse "off x=6..-48,y=40..-13,z=35..-12"
                c =! (Off, {  
                        x0 = -48
                        x1 =   6
                        y0 = -13
                        y1 =  40
                        z0 = -12
                        z1 =  35 })
            }
        ]
        testList "generating borders" [
            test "1 cube makes 2 borders in each dimension" {
                let c1 = { x0 = 0; x1 = 5; y0 = 10; y1 = 15; z0 = 0; z1 = 0 }
                let bs = borders [c1]
                bs.x =! Set [0;6]
                bs.y =! Set [10;16]
                bs.z =! Set [0;1]
            }
            test "2 non intersecting cubes make 4 borders in each direction" {
                let c1 = { x0 = 0; x1 = 1; y0 = 0; y1 = 1; z0 = 0; z1 = 1 }
                let c2 = { x0 = 10; x1 = 11; y0 = 10; y1 = 11; z0 = 10; z1 = 11 }
                let bs = borders [c1;c2]
                bs.x =! Set [0;2;10;12]
                bs.y =! Set [0;2;10;12]
                bs.z =! Set [0;2;10;12]
            }
            test "2 intersecting cubes make correct borders in each diretion" {
                let c1 = { x0 = 0; x1 = 5; y0 = 0; y1 = 5; z0 = 0; z1 = 0 }
                let c2 = { x0 = 2; x1 = 4; y0 = 4; y1 = 5; z0 = -1; z1 = 8 }
                let bs = borders [c1;c2]
                bs.x =! Set [0;2;5;6]
                bs.y =! Set [0;4;6]
                bs.z =! Set [-1;0;1;9]
            }
        ]
        testList "iterating borders" [
            test "1 dimension with 3 borders makes 3 cubes" {
                let bs = 
                    {
                        x = Set [10;20]
                        y = Set [30;50;40]
                        z = Set [60;70]
                    }
                let boxes = mapBoxes id bs |> Set
                boxes.Count =! 2
                boxes.Contains {
                    x0 = 10 ; x1 = 20
                    y0 = 30 ; y1 = 40
                    z0 = 60 ; z1 = 70 } =! true
                boxes.Contains {
                    x0 = 10 ; x1 = 20
                    y0 = 40 ; y1 = 50
                    z0 = 60 ; z1 = 70 } =! true
            }
        ]
        testList "solve" [
            test "part 1 demo" {
                solve demoinput |> sprintf "%A" =! "590784"
            }
            test "part 2 demo" {
                solve2 demoinput2 |> sprintf "%A" =! "2758514936282235UL"
            }
        ]
    ]


