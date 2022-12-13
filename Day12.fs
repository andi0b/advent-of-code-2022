module aoc22.Day12

type Vector = (struct (int * int))

module Vector =
    let add ((a1, b1): Vector) ((a2, b2): Vector) = struct (a1 + a2, b1 + b2)

type FVector = int64

module FVector =
    let add (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)
    let create x y = ((int64 x) <<< 32) ||| (int64 y)

type HMap = string array

type Step =
    | Steps of Vector list
    | NoSteps
    | Goal

module HMap =
    let at (hMap: HMap) ((x, y): Vector) = hMap |> Seq.item x |> Seq.item y

    let tryAt (hMap: HMap) ((x, y): Vector) =
        hMap |> Seq.tryItem x |> Option.bind (Seq.tryItem y)

    let canStep hMap from target =
        match (at hMap from, tryAt hMap target) with
        | _, None -> false
        | 'z', Some 'E' -> true
        | 'y', Some 'E' -> true
        | 'S', Some _ -> true
        | _, Some 'E' -> false
        | fEl, Some tEl -> tEl <= (fEl + (char 1))

    let possibleSteps hMap from =
        let possibleSteps =
            [ struct (1, 0); struct (-1, 0); struct (0, 1); struct (0, -1) ]
            |> Seq.map (Vector.add from)
            |> Seq.filter (canStep hMap from)
            |> Seq.toList

        match possibleSteps with
        | [] -> NoSteps
        | l when l |> List.exists (at hMap >> ((=) 'E')) -> Goal
        | l -> Steps l

    let findStart (hMap: HMap) =
        hMap
        |> Seq.indexed
        |> Seq.pick (fun (x, c) -> c |> Seq.tryFindIndex ((=) 'S') |> Option.map (fun y -> struct (x, y)))

let part1 hMap =
    let start = HMap.findStart hMap

    (0, Set.empty, Set.ofList [ start ])
    |> List.unfold (fun (i, visited, points) ->
        let possibleSteps =
            points
            |> Seq.map (HMap.possibleSteps hMap)
            |> Seq.collect (function
                | Steps s -> s |> Seq.map Some 
                | NoSteps -> []
                | Goal -> [None])
            |> Set.ofSeq

        if (possibleSteps |> Set.contains None) then
            None // found goal in this step
        else
            let next = Set.difference (possibleSteps |> Seq.choose id |> Set.ofSeq) visited
            let nextVisited = Set.union visited next
            Some((), (i + 1, nextVisited, next)))
    |> Seq.length
    |> (+) 1


let part2 hMap = 0

let run () =
    let input = System.IO.File.ReadAllLines "inputs/day12.txt"
    $"Part1: {input |> part1} Part2: {input |> part2}"

module test =
    open Xunit
    open Swensen.Unquote

    let example = [| "Sabqponm"; "abcryxxl"; "accszExk"; "acctuvwj"; "abdefghi" |]

    [<Fact>]
    let ``Part 1`` () = part1 example =! 31
