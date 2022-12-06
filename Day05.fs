module aoc22.Day05

open System
open System.IO
open FSharp.Text.RegexProvider

type Move =
    { count: int
      from: int
      target: int }

    member x.fromIdx = x.from - 1
    member x.targetIdx = x.target - 1

type Stacks = char list list

module Stacks =
    let topMessage (stacks: Stacks) =
        String(stacks |> List.map List.head |> Array.ofList)

    let applyMove reorder (stacks: Stacks) (move: Move) =
        let take, keep = stacks.[move.fromIdx] |> List.splitAt move.count

        stacks
        |> List.updateAt move.fromIdx keep
        |> List.updateAt move.targetIdx ((take |> reorder) @ stacks.[move.targetIdx])

let solve reorder initialStacks moves =
    moves |> List.fold (Stacks.applyMove reorder) initialStacks |> Stacks.topMessage

let part1 = solve List.rev
let part2 = solve id

module Parser =
    let Nl = Environment.NewLine
    type MoveRegex = Regex< @"^move (?<count>\d*) from (?<from>\d*) to (?<target>\d*)$" >

    let parseMoves (lines: string) =
        lines.Split Nl
        |> Array.map (
            MoveRegex().TypedMatch
            >> (fun m ->
                { count = m.count.Value |> int
                  from = m.from.Value |> int
                  target = m.target.Value |> int })
        )
        |> Array.toList

    let parseStacks (lines: string) =
        let extractCharsFromLine = Seq.chunkBySize 4 >> Seq.map (Seq.item 1) >> Seq.toList

        lines.Split Nl
        |> Seq.map extractCharsFromLine
        |> Seq.toList
        |> List.transpose
        |> List.map (List.filter Char.IsLetter)

    let parse (str: string) =
        match str.Split(Nl + Nl) with
        | [| stacksStr; movesStr |] -> (parseStacks stacksStr, parseMoves movesStr)
        | _ -> failwith "invalid input"

let run () =
    let input = File.ReadAllText("inputs/day05.txt") |> Parser.parse
    $"Part 1: {input ||> part1} Part2: {input ||> part2}"

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
        """    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"""

    let stacks, moves = example |> Parser.parse

    [<Fact>]
    let ``assert parsed stacks`` () =
        test <@ stacks = [ [ 'N'; 'Z' ]; [ 'D'; 'C'; 'M' ]; [ 'P' ] ] @>

    [<Fact>]
    let ``assert parsed moves`` () =
        test
            <@
                moves = [ { count = 1; from = 2; target = 1 }
                          { count = 3; from = 1; target = 3 }
                          { count = 2; from = 2; target = 1 }
                          { count = 1; from = 1; target = 2 } ]
            @>

    [<Fact>]
    let ``initial top message should be NDP`` () =
        test <@ Stacks.topMessage stacks = "NDP" @>

    [<Fact>]
    let ``part 1 should return CMZ`` () = test <@ part1 stacks moves = "CMZ" @>

    [<Fact>]
    let ``part 2 should return MCD`` () = test <@ part2 stacks moves = "MCD" @>
