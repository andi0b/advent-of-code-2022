module aoc22.Day09

type Instruction = Instruction of int * int

module Instruction =
    let parseInstructions (lines: string array) =
        let parseInstruction (line: string) =
            match line.[0] with
            | 'R' -> Instruction(1, 0)
            | 'L' -> Instruction(-1, 0)
            | 'D' -> Instruction(0, 1)
            | 'U' -> Instruction(0, -1)
            | x -> failwith $"Unknown instruction: {x}"
            |> Array.replicate (line.Substring(2) |> int)

        lines |> Array.map parseInstruction |> Array.collect id

module Vector =
    let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

    let touches (x1, y1) (x2, y2) =
        (x1 - x2) |> abs <= 1 && (y1 - y2) |> abs <= 1

    let sameRowOrColumn (x1, y1) (x2, y2) = x1 = x2 || y1 = y2

let findNewTail newHead oldTail =
    let newTailCandidates =
        seq {
            yield oldTail

            yield!
                if Vector.sameRowOrColumn oldTail newHead then
                    [| (1, 0); (-1, 0); (0, 1); (0, -1) |] // straight
                else
                    [| (1, 1); (-1, -1); (-1, 1); (1, -1) |] // diagonal
                |> Array.map (Vector.add oldTail)
        }

    let (>>&) f g x = f x && g x

    newTailCandidates
    |> Seq.find (Vector.touches newHead >>& Vector.touches oldTail)

let move (segments: (int * int) list) (Instruction(ix, iy)) =
    let newHead = segments.Head |> Vector.add (ix, iy)
    ((newHead, segments.Tail) ||> List.scan findNewTail)

let solve ropeLength instructions =
    let startSegments = (0, 0) |> List.replicate ropeLength
    let allPositions = (startSegments, instructions) ||> Array.scan move
    allPositions |> Seq.map List.last |> Seq.distinct |> Seq.length

let part1 = solve 2
let part2 = solve 10

let run () =
    let input =
        System.IO.File.ReadAllLines "inputs/day09.txt" |> Instruction.parseInstructions

    $"Part1: {input |> part1} Part2: {input |> part2}"

module tests =
    open Swensen.Unquote
    open Xunit

    let example1 =
        [| "R 4"; "U 4"; "L 3"; "D 1"; "R 4"; "D 1"; "L 5"; "R 2" |]
        |> Instruction.parseInstructions

    [<Fact>]
    let ``Part 1`` () = part1 example1 =! 13

    let example2 =
        [| "R 5"; "U 8"; "L 8"; "D 3"; "R 17"; "D 10"; "L 25"; "U 20" |]
        |> Instruction.parseInstructions

    [<Fact>]
    let ``Part 2`` () = part2 example2 =! 36
