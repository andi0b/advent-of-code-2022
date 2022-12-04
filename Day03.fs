module aoc22.Day03

open System
open System.IO
open Swensen.Unquote
open Xunit

type Item = char

module Item =
    let getPriority (item: Item) =
        if Char.IsLower(item) then (int item) - (int 'a') + 1
        elif Char.IsUpper(item) then (int item) - (int 'A') + 27
        else failwith "Unknown Item"

type Rucksack = Item array

module Rucksack =
    let parse (str: string) = str.ToCharArray()

    let getCompartments (rucksack: Rucksack) =
        (rucksack[0 .. rucksack.Length / 2 - 1], rucksack[rucksack.Length / 2 ..])

    let intersectCompartments rucksack =
        let c1, c2 = getCompartments rucksack
        Set.intersect (Set.ofArray c1) (Set.ofArray c2)

let part1 =
    Array.sumBy (fun r -> Rucksack.intersectCompartments r |> Seq.sumBy Item.getPriority)

let part2 (rucksacks: Rucksack array) =
    let elfSquads = rucksacks |> Seq.chunkBySize 3
    let badgeForSquad = Array.map Set.ofSeq >> Set.intersectMany >> Seq.exactlyOne
    elfSquads |> Seq.sumBy (badgeForSquad >> Item.getPriority)

let run () =
    let input = File.ReadAllLines("inputs/day03.txt") |> Array.map Rucksack.parse
    $"Part 1: {part1 input} Part2: {part2 input}"

module tests =
    let examples =
        [| "vJrwpWtwJgWrhcsFMMfFFhFp"
           "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
           "PmmdzqPrVvPwwTWBwg"
           "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
           "ttgJtRGJQctTZtZT"
           "CrZsJsPPZsGzwwsLwLmpwMDw" |]
        |> Array.map Rucksack.parse


    [<Fact>]
    let ``example part 1`` () = test <@ part1 examples = 157 @>

    [<Fact>]
    let ``example part 2`` () = test <@ part2 examples = 70 @>
