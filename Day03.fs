module aoc22.Day03

open System
open System.IO
open Swensen.Unquote
open Xunit

type Item = char

module Item =
    let getPriority =
        function
        | i when Char.IsLower(i) -> (int i) - (int 'a') + 1
        | i when Char.IsUpper(i) -> (int i) - (int 'A') + 27
        | _ -> failwith "Unknown Item"

type Rucksack = Item array

module Rucksack =
    let parse (str: string) = str.ToCharArray()
    let getCompartmentsContent = Array.splitInto 2

    let commonCompartmentItems =
        getCompartmentsContent >> Array.map Set.ofArray >> Set.intersectMany

let part1 (rucksacks: Rucksack seq) =
    rucksacks
    |> Seq.sumBy (Rucksack.commonCompartmentItems >> Seq.sumBy Item.getPriority)

let part2 (rucksacks: Rucksack seq) =
    let elfSquads = rucksacks |> Seq.chunkBySize 3
    let findSquadBadge = Seq.map Set.ofSeq >> Set.intersectMany >> Seq.exactlyOne
    elfSquads |> Seq.sumBy (findSquadBadge >> Item.getPriority)

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
