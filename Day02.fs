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
    // pick one shape, the it defeats the one to the left and forfeits against the one to the right
    let orderedShapes = [ Rock; Paper; Scissor ]

    let getDefeatForfeit shape =
        let id = orderedShapes |> List.findIndex (fun x -> x = shape)
        (orderedShapes.[(id + 3 - 1) % 3], orderedShapes.[(id + 1) % 3])

    let resultFromMoves theirMove myMove =
        if myMove = theirMove then
            Draw
        else
            let defeats, _ = getDefeatForfeit theirMove
            if myMove = defeats then Lose else Win

    let moveFromExpectedResult they expectedResult =
        let defeats, forfeits = getDefeatForfeit they

        match expectedResult with
        | Draw -> they
        | Win -> forfeits
        | Lose -> defeats

    let score myMove result =
        (match result with
         | Win -> 6
         | Draw -> 3
         | Lose -> 0)
        + (match myMove with
           | Rock -> 1
           | Paper -> 2
           | Scissor -> 3)

module Parser =
    let shapes = [| Rock; Paper; Scissor |]
    let gameResults = [| Lose; Draw; Win |]

    let parseShape1 (x: char) = shapes.[int (x - 'A')]
    let parseShape2 (x: char) = shapes.[int (x - 'X')]
    let parseGameResult (x: char) = gameResults.[int (x - 'X')]

    let splitLine (line: string) = (line.[0], line.[2])

    let parsePart1 (lines: string array) =
        lines |> Array.map (splitLine >> (fun (a, b) -> (parseShape1 a, parseShape2 b)))

    let parsePart2 (lines: string array) =
        lines
        |> Array.map (splitLine >> (fun (a, b) -> (parseShape1 a, parseGameResult b)))

let part1 input =
    let moves = Parser.parsePart1 input

    let moveScore (they, me) =
        let result = Game.resultFromMoves they me
        Game.score me result

    moves |> Array.map moveScore |> Array.sum

let part2 input =
    let moves = Parser.parsePart2 input

    let moveScore (they, expectedResult) =
        let myMove = Game.moveFromExpectedResult they expectedResult
        Game.score myMove expectedResult

    moves |> Array.map moveScore |> Array.sum

let run () =
    let input = File.ReadAllLines("inputs/day02.txt")
    $"Part 1: {part1 input} Part2: {part2 input}"

module tests =
    let example = [| "A Y"; "B X"; "C Z" |]

    [<Fact>]
    let ``parse example for part 1`` () =
        test <@ example |> Parser.parsePart1 = [| (Rock, Paper); (Paper, Rock); (Scissor, Scissor) |] @>


    [<Fact>]
    let ``part 1 results should be Win,Lose,Draw`` () =
        test
            <@ example
               |> Parser.parsePart1
               |> Array.map (fun (they, me) -> Game.resultFromMoves they me) = [| Win; Lose; Draw |] @>

    [<Fact>]
    let ``part 1 example output should be 15`` () = test <@ part1 example = 15 @>


    [<Fact>]
    let ``parse example for part 2`` () =
        test <@ example |> Parser.parsePart2 = [| (Rock, Draw); (Paper, Lose); (Scissor, Win) |] @>

    [<Fact>]
    let ``part 2 moves should be Rock,Rock,Rock`` () =
        test
            <@ example
               |> Parser.parsePart2
               |> Array.map (fun (they, expectedResult) -> Game.moveFromExpectedResult they expectedResult) = [| Rock
                                                                                                                 Rock
                                                                                                                 Rock |] @>

    [<Fact>]
    let ``part 2 example output should be 12`` () = test <@ part2 example = 12 @>
