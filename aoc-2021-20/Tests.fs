module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput
open System

[<Tests>]
let all =
    testList "all" [
        testList "parsing" [
            test "demoinput" {
                let image = parse demoinput
                image.bits.Count =! 512
                image.bits.Item 0 =! 0
                image.bits.Item 2 =! 1
                image.bits.Item 511 =! 1

                Image.countCells image =! 10
                Image.containsPixel (0,0) image =! true
                Image.containsPixel (1,0) image =! false
                Image.containsPixel (4,4) image =! true
            }
        ]
        testList "enhancing" [
            test "single point" {
                let input = parse demoinput
                enhancePoint (2,2) input =! 1
            }
            test "whole image" {
                let image = demoinput |> parse |> enhanceImage
                Image.containsPixel (-1,-1) image =! false
                Image.containsPixel ( 0,-1) image =! true
                Image.containsPixel ( 1,-1) image =! true
                Image.containsPixel ( 2,-1) image =! false
                Image.countCells image =! 24
                Image.containsPixel (-2,-2) image =! false
            }
            test "whole image with infinite pixels getting #" {
                let image =
                    demoinput
                    |> parse
                    |> fun x -> { x with bits = x.bits.Add(0,1) }
                    |> enhanceImage
                Image.containsPixel (-2,-2) image =! true
            }
            test "solve demoinput" {
                solve demoinput =! 35
            }
            test "solve demoinput part 2" {
                solve2 demoinput =! 3351
            }
        ]

    ]


