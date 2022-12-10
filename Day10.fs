module aoc22.Day10

open System

type CpuState = { X: int }

let (|Prefix|_|) (p: string) (s: string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let (|AddX|Noop|Unknown|) str =
    match str with
    | "noop" -> Noop
    | Prefix "addx " rest -> AddX(rest |> int)
    | _ -> Unknown

let execute (instruction: string) cpuState =
    match instruction with
    | Noop -> [| cpuState |]
    | AddX x -> [| cpuState; { cpuState with X = cpuState.X + x } |]
    | u -> failwith $"unknown command: {u}"

let runProgram instructions =
    ([| { X = 1 } |], instructions)
    ||> Array.scan (fun state instruction -> execute instruction (state |> Array.last))
    |> Array.collect id

let isSignificantIndex i = ((i - 20) % 40) = 0

let part1 instructions =
    let states = runProgram instructions

    states
    |> Array.mapi (fun i s -> if (i + 1) |> isSignificantIndex then ((i + 1) * s.X) else 0)
    |> Array.sum

let part2 instructions =
    let states = runProgram instructions

    let output =
        states
        |> Array.skip 0
        |> Array.mapi (fun i { X = x } -> if (abs (i % 40 - x)) <= 1 then '#' else ' ')

    output
    |> Array.chunkBySize 40
    |> Array.map String
    |> Array.reduce (fun t n -> t + Environment.NewLine + n)

let run () =
    let input = System.IO.File.ReadAllLines "inputs/day10.txt"

    $"Part1: {input |> part1} Part2: {Environment.NewLine}{input |> part2}"

module test =
    open Xunit
    open Swensen.Unquote

    let shortExample = [| "noop"; "addx 3"; "addx -5" |]

    [<Fact>]
    let ``Short example`` () =
        runProgram shortExample
        =! [| { X = 1 }; { X = 1 }; { X = 1 }; { X = 4 }; { X = 4 }; { X = -1 } |]

    [<Fact>]
    let ``Significant index 20th, 60th, 100th, 140th, 180th, and 220th `` () =
        [ 0..250 ] |> List.filter isSignificantIndex =! [ 20; 60; 100; 140; 180; 220 ]

    let example =
        [| "addx 15"
           "addx -11"
           "addx 6"
           "addx -3"
           "addx 5"
           "addx -1"
           "addx -8"
           "addx 13"
           "addx 4"
           "noop"
           "addx -1"
           "addx 5"
           "addx -1"
           "addx 5"
           "addx -1"
           "addx 5"
           "addx -1"
           "addx 5"
           "addx -1"
           "addx -35"
           "addx 1"
           "addx 24"
           "addx -19"
           "addx 1"
           "addx 16"
           "addx -11"
           "noop"
           "noop"
           "addx 21"
           "addx -15"
           "noop"
           "noop"
           "addx -3"
           "addx 9"
           "addx 1"
           "addx -3"
           "addx 8"
           "addx 1"
           "addx 5"
           "noop"
           "noop"
           "noop"
           "noop"
           "noop"
           "addx -36"
           "noop"
           "addx 1"
           "addx 7"
           "noop"
           "noop"
           "noop"
           "addx 2"
           "addx 6"
           "noop"
           "noop"
           "noop"
           "noop"
           "noop"
           "addx 1"
           "noop"
           "noop"
           "addx 7"
           "addx 1"
           "noop"
           "addx -13"
           "addx 13"
           "addx 7"
           "noop"
           "addx 1"
           "addx -33"
           "noop"
           "noop"
           "noop"
           "addx 2"
           "noop"
           "noop"
           "noop"
           "addx 8"
           "noop"
           "addx -1"
           "addx 2"
           "addx 1"
           "noop"
           "addx 17"
           "addx -9"
           "addx 1"
           "addx 1"
           "addx -3"
           "addx 11"
           "noop"
           "noop"
           "addx 1"
           "noop"
           "addx 1"
           "noop"
           "noop"
           "addx -13"
           "addx -19"
           "addx 1"
           "addx 3"
           "addx 26"
           "addx -30"
           "addx 12"
           "addx -1"
           "addx 3"
           "addx 1"
           "noop"
           "noop"
           "noop"
           "addx -9"
           "addx 18"
           "addx 1"
           "addx 2"
           "noop"
           "noop"
           "addx 9"
           "noop"
           "noop"
           "noop"
           "addx -1"
           "addx 2"
           "addx -37"
           "addx 1"
           "addx 3"
           "noop"
           "addx 15"
           "addx -21"
           "addx 22"
           "addx -6"
           "addx 1"
           "noop"
           "addx 2"
           "addx 1"
           "noop"
           "addx -10"
           "noop"
           "noop"
           "addx 20"
           "addx 1"
           "addx 2"
           "addx 2"
           "addx -6"
           "addx -11"
           "noop"
           "noop"
           "noop" |]

    [<Fact>]
    let ``Part 1`` () = part1 example =! 13140
