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
                let image = parseInput demoinput
                image.enhancementRules.Count =! 512
                image.enhancementRules.Item 0 =! 0
                image.enhancementRules.Item 2 =! 1
                image.enhancementRules.Item 511 =! 1

                image.pixels.Count =! 10
                containsPixel image (0,0) =! true
                containsPixel image (1,0) =! false
                containsPixel image (4,4) =! true
            }
        ]
        testList "enhancing" [
            test "single point" {
                let input = parseInput demoinput
                enhancePoint input (2,2) =! true
            }
            test "whole image" {
                let image = demoinput |> parseInput |> enhanceImage
                containsPixel image (-1,-1) =! false
                containsPixel image ( 0,-1) =! true
                containsPixel image ( 1,-1) =! true
                containsPixel image ( 2,-1) =! false
                image.pixels.Count =! 24
                containsPixel image (-2,-2) =! false
            }
            test "whole image with infinite pixels getting #" {
                let image =
                    demoinput
                    |> parseInput
                    |> fun x -> { x with enhancementRules = x.enhancementRules.Add(0,1) }
                    |> enhanceImage
                containsPixel image (-2,-2) =! true
            }
            test "solve demoinput" {
                solve demoinput =! 35
            }
            // using ptest to skip slow test
            ptest "solve demoinput part 2" {
                solve2 demoinput =! 3351
            }
        ]

    ]


