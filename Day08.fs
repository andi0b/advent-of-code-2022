module aoc22.Day08

open System

let parseForrest: string array -> int array array =
    Array.map (Seq.map (Char.ToString >> int) >> Seq.toArray)

type Los = { start: int * int; vector: int * int }

module Vector =
    let add (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

module Forrest =

    let size forrest =
        (forrest |> Array.length, forrest.[0] |> Array.length)

    let cornerLos forrest =
        let lenA, lenB = forrest |> size

        [ [ 0..lenB ] |> List.map (fun b -> { start = (-1, b); vector = (1, 0) })
          [ 0..lenB ] |> List.map (fun b -> { start = (lenA, b); vector = (-1, 0) })
          [ 0..lenA ] |> List.map (fun a -> { start = (a, -1); vector = (0, 1) })
          [ 0..lenA ] |> List.map (fun a -> { start = (a, lenB); vector = (0, -1) }) ]
        |> List.collect id

    let treesLos forrest (los: Los) =
        let lenA, lenB = forrest |> size

        let oob (a, b) = // check point for out of bounds
            a < 0 || a >= lenA || b < 0 || b >= lenB

        los.start
        |> List.unfold (fun p ->
            let next = p |> Vector.add los.vector
            if oob next then None else Some(next, next))

    let visibleTreesLos forrest (los: Los) =
        (([], -1), treesLos forrest los)
        ||> List.fold (fun (visible, maxHeight) (a, b) ->
            if forrest.[a].[b] > maxHeight then
                ((a, b) :: visible, forrest.[a].[b])
            else
                (visible, maxHeight))
        |> fst

    let treesNotHigherThan height forrest (los: Los) =
        let losTrees = treesLos forrest los

        let blockingIdx =
            losTrees |> List.tryFindIndex (fun (a, b) -> forrest.[a].[b] >= height)

        match blockingIdx with
        | None -> losTrees
        | Some i -> losTrees |> List.take (i + 1 |> min losTrees.Length)

    let scenicScore (forrest: int[][]) (a, b) =
        let los =
            [ (0, 1); (0, -1); (1, 0); (-1, 0) ]
            |> List.map (fun v -> { start = (a, b); vector = v })

        los
        |> List.map (treesNotHigherThan forrest.[a].[b] forrest)
        |> List.map (List.length)
        |> List.reduce (*)

let part1 forrest =
    forrest
    |> Forrest.cornerLos
    |> List.map (Forrest.visibleTreesLos forrest)
    |> List.collect id
    |> List.distinct
    |> List.length

let part2 (forrest: int[][]) =
    let lenA, lenB = forrest |> Forrest.size
    let allPos = List.allPairs [ 0 .. lenA - 1 ] [ 0 .. lenB - 1 ]

    allPos |> List.map (Forrest.scenicScore forrest) |> List.max

let run () =
    let forrest = System.IO.File.ReadAllLines "inputs/day08.txt" |> parseForrest
    $"Part1: {forrest |> part1} Part2: {forrest |> part2}"

module tests =
    open Swensen.Unquote
    open Xunit

    let example = [| "30373"; "25512"; "65332"; "33549"; "35390" |] |> parseForrest

    [<Fact>]
    let ``Part 1`` () = part1 example =! 21

    [<Fact>]
    let ``Part 2`` () = part2 example =! 8
