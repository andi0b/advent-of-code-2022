module aoc22.Day18

open System

type Vec3 = (struct (int * int * int))

module Vec3 =
    let parse (str: string) : Vec3 =
        let p = str.Split(",") |> Array.map int
        p.[0], p.[1], p.[2]

    let all a : Vec3 = struct (a, a, a)

    let inline each func ((ax, ay, az): Vec3) ((bx, by, bz): Vec3) : Vec3 = (func ax bx, func ay by, func az bz)

    let add = each (+)
    let min = each min
    let max = each max

    let allCombinations (xs: int seq) (ys: int seq) (zs: int seq) : Vec3 Set =
        Seq.allPairs xs (Seq.allPairs ys zs)
        |> Seq.map (fun (x, (y, z)) -> struct (x, y, z))
        |> Set.ofSeq

    let cubeSurface ((x1, y1, z1): Vec3) ((x2, y2, z2): Vec3) =
        [ allCombinations [ x1; x2 ] [ y1..y2 ] [ z1..z2 ]
          allCombinations [ x1..x2 ] [ y1; y2 ] [ z1..z2 ]
          allCombinations [ x1..x2 ] [ y1..y2 ] [ z1; z2 ] ]
        |> Set.unionMany

type CubeSet = Vec3 Set

module CubeSet =
    let parse = Array.map Vec3.parse >> Set.ofArray

    let surfaceDirections: Vec3 array =
        [| (-1, 0, 0); (1, 0, 0); (0, -1, 0); (0, 1, 0); (0, 0, 1); (0, 0, -1) |]

    let adjacentPositions cube =
        surfaceDirections |> Seq.map (Vec3.add cube) |> Set.ofSeq

    let minMax set =
        ((Vec3.all Int32.MaxValue, Vec3.all Int32.MinValue), set)
        ||> Seq.fold (fun (minPos, maxPos) next -> (Vec3.min minPos next, Vec3.max maxPos next))


let part1 set =
    let openSurfaceCount (cube: Vec3) =
        let intersections = set |> Set.intersect (CubeSet.adjacentPositions cube)
        6 - intersections.Count

    set |> Seq.sumBy openSurfaceCount

let part2 set =
    let min, max = CubeSet.minMax set

    let withOffset offset =
        (min |> Vec3.add (Vec3.all -offset), max |> Vec3.add (Vec3.all offset))

    let off1 = withOffset 1 ||> Vec3.cubeSurface
    let off2 = withOffset 2 ||> Vec3.cubeSurface

    let water =
        (off2, off1)
        |> Seq.unfold (fun (visited, cur) ->

            let next =
                cur
                |> Seq.collect CubeSet.adjacentPositions
                |> Set.ofSeq
                |> (fun x -> Set.difference x set)
                |> (fun x -> Set.difference x visited)

            let retVal = ((Set.union visited cur), next)

            if next.Count > 0 then Some(retVal, retVal) else None)
        |> Seq.last
        |> fst

    set
    |> Seq.sumBy (fun cube -> cube |> CubeSet.adjacentPositions |> Set.intersect water |> Set.count)



let run () =
    let input = System.IO.File.ReadAllLines "inputs/day18.txt" |> CubeSet.parse
    $"Part1: {input |> part1} Part2: {input |> part2}"

module test =
    open Xunit
    open Swensen.Unquote

    let example =
        [| "2,2,2"
           "1,2,2"
           "3,2,2"
           "2,1,2"
           "2,3,2"
           "2,2,1"
           "2,2,3"
           "2,2,4"
           "2,2,6"
           "1,2,5"
           "3,2,5"
           "2,1,5"
           "2,3,5" |]
        |> CubeSet.parse

    [<Fact>]
    let ``Part 1`` () = part1 example =! 64

    [<Fact>]
    let ``Part 2`` () = part2 example =! 58
