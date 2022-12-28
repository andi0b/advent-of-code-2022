module aoc22.Day19

// Ore * Clay * Obsidian * Geode
type Materials = int * int * int * int

module Materials =
    let all (f: int -> int -> 'a) ((a1, a2, a3, a4): Materials) ((b1, b2, b3, b4): Materials) =
        (f a1 b1, f a2 b2, f a3 b3, f a4 b4)

    let toSeq ((a, b, c, d): 'a * 'a * 'a * 'a) =
        seq {
            a
            b
            c
            d
        }

    let gte (a: Materials) (b: Materials) =
        all (>=) a b |> toSeq |> Seq.reduce (&&)

    let lte (a: Materials) (b: Materials) =
        all (<=) a b |> toSeq |> Seq.reduce (&&)

    let geodes ((_, _, _, g): Materials) = g

    let zero: Materials = (0, 0, 0, 0)

type BlueprintEntry =
    { Needs: Materials
      Produces: Materials }

type Robots = Robots of Materials

type Collected = Collected of Materials

type Blueprint = BlueprintEntry list

let harvest (Collected c) (Robots r) = Collected(Materials.all (+) c r)

let buildingOptions blueprints (Collected c) (waitingFor: int list) =
    let canBuild =
        blueprints
        |> List.indexed
        |> (fun l ->
            match waitingFor with
            | [] -> l
            | _ -> l |> List.filter (fun (i, _) -> waitingFor |> List.contains i))
        |> List.map (fun (i, e) ->
            if Materials.gte c e.Needs then
                let collectedAfterProduction = Materials.all (-) c e.Needs
                Ok(Collected collectedAfterProduction, Robots e.Produces, [])
            else
                Error i)

    let waitingFor =
        canBuild
        |> List.choose (function
            | Error i -> Some i
            | _ -> None)

    let buildNothing =
        match waitingFor with
        | [] -> []
        | _ -> [ (Collected c, Robots Materials.zero, waitingFor) ]

    buildNothing @ (canBuild |> List.choose Result.toOption)

let simulateOneMinute blueprint (Collected c) (Robots r) waitingFor =
    buildingOptions blueprint (Collected c) waitingFor
    |> Seq.map (fun (Collected collectedAfterBuilding, Robots builtRobots, waitingFor) ->
        let newCollected = Collected(Materials.all (+) collectedAfterBuilding r)
        let newRobots = Robots(Materials.all (+) builtRobots r)
        (newCollected, newRobots, waitingFor))
    |> Array.ofSeq

let inline triangular (n: int) = n * (n + 1) / 2

let parallelFilter condition =
    Array.Parallel.choose (fun x -> if condition x then Some x else None)

let rec simulateOneMinuteMany blueprint (states: (Collected * Robots * int list) array) i rounds =
    let remainingMinutes = rounds + 1 - i

    // remove configurations that can never reach the minimum goal
    let minimumPossibleGeodes =
        states
        |> Array.Parallel.map (fun (Collected c, Robots r, _) ->
            let openedGeodes = c |> Materials.geodes
            let geodeRobots = r |> Materials.geodes
            openedGeodes + geodeRobots * remainingMinutes)
        |> Seq.max

    let states' =
        if minimumPossibleGeodes > 0 then
            states
            |> parallelFilter (fun (Collected(_, _, _, gc), Robots(_, _, _, gr), _) ->
                gc + gr * remainingMinutes + (triangular remainingMinutes - 1)
                >= minimumPossibleGeodes)
        else
            states

    //printfn
    //    $"States reduced by {states.Length - states'.Length}, that don't match minimumPossibleGeodes: {minimumPossibleGeodes}"

    let nextStates =
        states'
        |> Array.Parallel.collect (fun (c, r, w) -> simulateOneMinute blueprint c r w)

    // don't build more lesser robots than we can use up materials of that type in one round
    let robotLimit =
        blueprint
        |> Seq.map (fun e -> e.Needs)
        |> Seq.reduce (fun a b -> Materials.all max a b)

    let nextStates' =
        nextStates
        |> parallelFilter (fun (_, Robots(a, b, c, _), _) -> Materials.lte (a, b, c, 0) robotLimit)
        |> Array.ofSeq

    //printfn $"Round {i}, simulations: {nextStates'.Length}"
    nextStates'

let simulateBlueprint rounds blueprint =
    let start = (Collected Materials.zero, Robots(1, 0, 0, 0), [])

    let possibleOutcomes =
        ([| start |], [ 1..rounds ])
        ||> Seq.fold (fun prevOutcomes i -> simulateOneMinuteMany blueprint prevOutcomes i rounds)

    let maxGeodes =
        possibleOutcomes
        |> Seq.map (fun (Collected c, _, _) -> Materials.geodes c)
        |> Seq.max

    maxGeodes

let solve blueprints rounds =
    blueprints |> List.map (simulateBlueprint rounds)


let part1 blueprints =
    solve blueprints 24
    |> List.indexed
    |> List.map (fun (id, geodes) -> (id + 1) * geodes)
    |> List.sum

let part2 blueprints =
    solve (blueprints |> List.truncate 3) 32 |> Seq.reduce (*)

let parse str =
    match tryRegexG "([0-9]+)" str with
    | Some m when m.Length = 7 ->
        let nums = m |> List.map int

        [ // Each ore robot costs #1 ore
          { Needs = (nums.[1], 0, 0, 0)
            Produces = (1, 0, 0, 0) }

          // Each clay robot costs #2 ore
          { Needs = (nums.[2], 0, 0, 0)
            Produces = (0, 1, 0, 0) }

          // Each obsidian robot costs #3 ore and #4 clay
          { Needs = (nums.[3], nums.[4], 0, 0)
            Produces = (0, 0, 1, 0) }

          // Each geode robot costs #5 ore and #6 obsidian
          { Needs = (nums.[5], 0, nums.[6], 0)
            Produces = (0, 0, 0, 1) } ]
    | _ -> failwith $"can't parse blueprint: {str}"

let run () =
    let input =
        System.IO.File.ReadAllLines "inputs/day19.txt"
        |> Array.map parse
        |> List.ofArray

    $"Part1: {input |> part1} Part2: {input |> part2}"

module tests =
    open Xunit
    open Swensen.Unquote

    let example =
        [ "Blueprint 1:  Each ore robot costs 4 ore.  Each clay robot costs 2 ore.  Each obsidian robot costs 3 ore and 14 clay.  Each geode robot costs 2 ore and 7 obsidian."
          "Blueprint 2:  Each ore robot costs 2 ore.  Each clay robot costs 3 ore.  Each obsidian robot costs 3 ore and 8 clay.  Each geode robot costs 3 ore and 12 obsidian." ]
        |> List.map parse

    [<Fact>]
    let gteTest () =
        Materials.gte (1, 1, 1, 1) (0, 0, 0, 0) =! true
        Materials.gte (1, 1, 1, 1) (1, 1, 1, 1) =! true
        Materials.gte (2, 1, 1, 1) (1, 1, 1, 1) =! true
        Materials.gte (1, 1, 1, 1) (1, 1, 1, 2) =! false
        Materials.gte (0, 0, 0, 0) (1, 1, 1, 1) =! false

    [<Fact>]
    let ``Parse Test`` () =
        example.[0]
        =! [ // Each ore robot costs 4 ore
             { Needs = (4, 0, 0, 0)
               Produces = (1, 0, 0, 0) }

             // Each clay robot costs 2 ore
             { Needs = (2, 0, 0, 0)
               Produces = (0, 1, 0, 0) }

             // Each obsidian robot costs 3 ore and 14 clay
             { Needs = (3, 14, 0, 0)
               Produces = (0, 0, 1, 0) }

             // Each geode robot costs 2 ore and 7 obsidian
             { Needs = (2, 0, 7, 0)
               Produces = (0, 0, 0, 1) } ]

    [<Fact>]
    let ``Part 1`` () = part1 example =! 33

    [<Fact>]
    let ``Part 2`` () = part2 example =! 56 * 62
