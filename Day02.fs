module aoc22.Day02

open System.IO
open Swensen.Unquote
open Xunit

type Shape =
    | Rock
    | Paper
    | Scissor

type GameResult =
    | Win
    | Lose
    | Draw

module Game =
    let getShapeFromOffset offset shape =
        // pick one shape, it defeats the one to the left and forfeits against the one to the right
        let orderedShapes = [ Rock; Paper; Scissor ]
        let id = orderedShapes |> List.findIndex (fun x -> x = shape)
        orderedShapes.[(id + 3 + offset) % 3]

    let getWinningShape = getShapeFromOffset 1
    let getLosingShape = getShapeFromOffset -1

    let resultFromMoves theirShape myShape =
        if myShape = theirShape then Draw
        else if myShape = (theirShape |> getLosingShape) then Lose
        else Win

    let moveFromExpectedResult theirShape expectedResult =
        match expectedResult with
        | Draw -> theirShape
        | Win -> theirShape |> getWinningShape
        | Lose -> theirShape |> getLosingShape

    let score myMove gameResult =
        (match gameResult with
         | Win -> 6
         | Draw -> 3
         | Lose -> 0)
        + (match myMove with
           | Rock -> 1
           | Paper -> 2
           | Scissor -> 3)

module Parser =
    let shapes = [| Rock; Paper; Scissor |]
    let parseShape1 chr = shapes.[int (chr - 'A')]
    let parseShape2 chr = shapes.[int (chr - 'X')]
    let parseGameResult chr = [| Lose; Draw; Win |].[int (chr - 'X')]
    let splitLine map1 map2 (line: string) = (line.[0] |> map1, line.[2] |> map2)

    let parsePart1 = Array.map (splitLine parseShape1 parseShape2)
    let parsePart2 = Array.map (splitLine parseShape1 parseGameResult)

let part1 input =
    let moves = Parser.parsePart1 input

    let scoreMove (theirShape, me) =
        let result = Game.resultFromMoves theirShape me
        Game.score me result

    moves |> Array.sumBy scoreMove

let part2 input =
    let moves = Parser.parsePart2 input

    let scoreMove (theirShape, expectedResult) =
        let myMove = Game.moveFromExpectedResult theirShape expectedResult
        Game.score myMove expectedResult

    moves |> Array.sumBy scoreMove

let run () =
    let input = File.ReadAllLines("inputs/day02.txt")
    $"Part 1: {part1 input} Part2: {part2 input}"

let example = [| "A Y"; "B X"; "C Z" |]

module ``Part 1 Tests`` =
    [<Fact>]
    let ``Parsing`` () =
        test <@ example |> Parser.parsePart1 = [| (Rock, Paper); (Paper, Rock); (Scissor, Scissor) |] @>

    [<Fact>]
    let ``round results should be Win,Lose,Draw`` () =
        test
            <@
                example
                |> Parser.parsePart1
                |> Array.map (fun (they, me) -> Game.resultFromMoves they me) = [| Win; Lose; Draw |]
            @>

    [<Fact>]
    let ``output should be 15`` () = test <@ part1 example = 15 @>

module ``Part 2 Tests`` =
    [<Fact>]
    let ``Parsing`` () =
        test <@ example |> Parser.parsePart2 = [| (Rock, Draw); (Paper, Lose); (Scissor, Win) |] @>

    [<Fact>]
    let ``my moves should be Rock,Rock,Rock`` () =
        test
            <@
                example
                |> Parser.parsePart2
                |> Array.map (fun (they, result) -> Game.moveFromExpectedResult they result) = [| Rock; Rock; Rock |]
            @>

    [<Fact>]
    let ``output should be 12`` () = test <@ part2 example = 12 @>
