module aoc22.Day04

open System
open System.IO
open Swensen.Unquote
open Xunit
open FSharp.Text.RegexProvider

type Assignment = int * int
type Pair = Assignment * Assignment

type PairRegex = Regex< @"^(\d*)-(\d*),(\d*)-(\d*)$" >

module Pair =
    let parse (str: string) : Pair =
        let m = PairRegex().TypedMatch(str)
        ((int m.``1``.Value, int m.``2``.Value), (int m.``3``.Value, int m.``4``.Value))

    let fullyContains ((x, y): Pair) =
        let aIncludedInB (aFrom, aTo) (bFrom, bTo) = aFrom >= bFrom && aTo <= bTo
        aIncludedInB x y || aIncludedInB y x

    let overlaps ((x, y): Pair) =
        let pointInside (aFrom, aTo) point = aFrom <= point && aTo >= point
        pointInside x (fst y) || pointInside x (snd y) || fullyContains (x, y)

let solve filterMethod = Seq.filter filterMethod >> Seq.length
let part1 = solve Pair.fullyContains
let part2 = solve Pair.overlaps

let run () =
    let input = File.ReadAllLines("inputs/day04.txt") |> Array.map Pair.parse
    $"Part 1: {part1 input} Part2: {part2 input}"

module tests =
    let examples =
        [| "2-4,6-8"; "2-3,4-5"; "5-7,7-9"; "2-8,3-7"; "6-6,4-6"; "2-6,4-8" |]
        |> Array.map Pair.parse

    [<Fact>]
    let ``example part 1`` () = test <@ part1 examples = 2 @>

    [<Fact>]
    let ``example part 2`` () = test <@ part2 examples = 4 @>
