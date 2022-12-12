module aoc22.Day11

type Monkey =
    { Operation: int64 -> int64
      DivisibleBy: int
      TrueMonkey: int
      FalseMonkey: int }

module Monkey =
    let calculateWorryLevel (divisor: int64) onlyWhenDivisible monkey level =
        let afterOp = (level |> monkey.Operation)

        if onlyWhenDivisible then
            afterOp % divisor
        else
            afterOp / divisor

type MonkeyGroup =
    { Monkeys: Monkey list
      Divisor: int
      OnlyWhenDivisible: bool }

type MonkeyState =
    { Items: int64 list
      InspectionCount: int }

module Game =
    let move group currentMonkeyId (states: MonkeyState list) =
        let monkey = group.Monkeys.[currentMonkeyId]
        let state = states.[currentMonkeyId]

        let itemsForOtherMonkeys =
            state.Items
            |> List.map (Monkey.calculateWorryLevel group.Divisor group.OnlyWhenDivisible monkey)
            |> List.map (fun wl ->
                (if wl % (int64 monkey.DivisibleBy) = 0L then
                     monkey.TrueMonkey
                 else
                     monkey.FalseMonkey),
                wl)

        states
        |> List.mapi (fun mid i ->
            if mid = currentMonkeyId then
                { state with
                    Items = []
                    InspectionCount = state.InspectionCount + state.Items.Length }
            else
                let newItems =
                    itemsForOtherMonkeys |> List.filter (fst >> ((=) mid)) |> List.map snd

                { states.[mid] with Items = (states.[mid].Items @ newItems) })

    let round group items =
        (items, group.Monkeys |> List.indexed)
        ||> List.fold (fun i (mid, m) -> move group mid i)

let parseOne (lines: string array) =
    let startingItems =
        match lines.[1] with
        | Prefix "  Starting items: " rest ->
            rest.Split ", "
            |> Seq.map int64
            |> Seq.toList
            |> (fun items -> { Items = items; InspectionCount = 0 })
        | _ -> failwith "parse err"

    let monkey =
        { Operation =
            match lines.[2] |> tryPrefix "  Operation: new = " with
            | Some(Prefix "old * old" _) -> (fun x -> pown x 2)
            | Some(Prefix "old * " rest) -> ((*) (rest |> int64))
            | Some(Prefix "old + " rest) -> ((+) (rest |> int64))
            | _ -> failwith $"unknown operation: {lines.[2]}"
          DivisibleBy =
            match lines.[3] with
            | Prefix "  Test: divisible by " rest -> rest |> int
            | _ -> failwith "div err"
          TrueMonkey =
            match lines.[4] with
            | Prefix "    If true: throw to monkey " rest -> rest |> int
            | _ -> failwith "div err"
          FalseMonkey =
            match lines.[5] with
            | Prefix "    If false: throw to monkey " rest -> rest |> int
            | _ -> failwith "div err" }

    (monkey, startingItems)

let parseAll (lines: string array) =
    let results =
        lines |> Array.toSeq |> Seq.chunkBySize 7 |> Seq.map parseOne |> Seq.toList

    (results |> List.map fst, results |> List.map snd)

let solve rounds (group: MonkeyGroup) (items: MonkeyState list) =
    let finalState =
        (items, [ 1..rounds ]) ||> List.fold (fun i _ -> Game.round group i)

    finalState
    |> List.map (fun s -> s.InspectionCount)
    |> List.sortDescending
    |> List.take 2
    |> List.map int64
    |> List.reduce (*)

let part1 (monkeys: Monkey list) (items: MonkeyState list) =
    let group =
        { Monkeys = monkeys
          Divisor = 3
          OnlyWhenDivisible = false }

    solve 20 group items

let part2 (monkeys: Monkey list) (items: MonkeyState list) =
    let divisor =
        monkeys |> List.map (fun m -> m.DivisibleBy) |> List.reduce (*)

    let group =
        { Monkeys = monkeys
          Divisor = divisor
          OnlyWhenDivisible = true }

    solve 10000 group items

let run () =
    let input = System.IO.File.ReadAllLines "inputs/day11.txt" |> parseAll
    $"Part1: {input ||> part1} Part2: {input ||> part2}"

module test =
    open Xunit
    open Swensen.Unquote

    let example =
        [| "Monkey 0:"
           "  Starting items: 79, 98"
           "  Operation: new = old * 19"
           "  Test: divisible by 23"
           "    If true: throw to monkey 2"
           "    If false: throw to monkey 3"
           ""
           "Monkey 1:"
           "  Starting items: 54, 65, 75, 74"
           "  Operation: new = old + 6"
           "  Test: divisible by 19"
           "    If true: throw to monkey 2"
           "    If false: throw to monkey 0"
           ""
           "Monkey 2:"
           "  Starting items: 79, 60, 97"
           "  Operation: new = old * old"
           "  Test: divisible by 13"
           "    If true: throw to monkey 1"
           "    If false: throw to monkey 3"
           ""
           "Monkey 3:"
           "  Starting items: 74"
           "  Operation: new = old + 3"
           "  Test: divisible by 17"
           "    If true: throw to monkey 0"
           "    If false: throw to monkey 1" |]

    [<Fact>]
    let ``parse test`` () =
        let monkeys, items = parseAll example

        items
        =! ([ [ 79; 98 ]; [ 54; 65; 75; 74 ]; [ 79; 60; 97 ]; [ 74 ] ]
            |> List.map (fun i ->
                { Items = i |> List.map int64
                  InspectionCount = 0 }))

        let m = monkeys.[3]
        m.DivisibleBy =! 17
        m.TrueMonkey =! 0
        m.FalseMonkey =! 1
        test <@ 11L |> m.Operation = 14L @>
        0

    [<Fact>]
    let ``Part 1`` () = parseAll example ||> part1 =! 10605L

    [<Fact>]
    let ``Part 2`` () =
        parseAll example ||> part2 =! 2713310158L
