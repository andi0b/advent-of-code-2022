module aoc22.Day01

open System
open System.IO
open Swensen.Unquote
open Xunit

let parse (x: string) =
    x.Split(Environment.NewLine + Environment.NewLine)
    |> Array.map (fun y -> y.Split(Environment.NewLine) |> Array.map Int32.Parse)

let totalCaloriesPerElf = Array.map Array.sum
let part1 = totalCaloriesPerElf >> Array.max
let part2 = totalCaloriesPerElf >> Array.sortDescending >> Array.take 3 >> Array.sum

let run () =
    let input = File.ReadAllText("inputs/day01.txt") |> parse
    $"Part 1: {part1 input} Part2: {part2 input}"

module tests =
    let example =
        parse
            """1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"""

    [<Fact>]
    let ``parse example`` () =
        test
            <@ example = [| [| 1000; 2000; 3000 |]
                            [| 4000 |]
                            [| 5000; 6000 |]
                            [| 7000; 8000; 9000 |]
                            [| 10000 |] |] @>

    [<Fact>]
    let ``part 1 example output should be 24000`` () = test <@ part1 example = 24000 @>

    [<Fact>]
    let ``part 2 example output should be 45000`` () = test <@ part2 example = 45000 @>
