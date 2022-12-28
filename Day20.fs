module aoc22.Day20

open System.Collections.Generic

let walk amount (node: 'a LinkedListNode) =
    let walkOne (node: 'a LinkedListNode) =
        if amount > 0 then
            if node.Next <> null then node.Next else node.List.First
        else if node.Previous <> null then
            node.Previous
        else
            node.List.Last

    (node, seq { 1 .. (abs amount) }) ||> Seq.fold (fun n i -> walkOne n)

let inline shuffle times (len: 'a) (input: 'a seq) =

    let list = LinkedList(input)

    let originalOrder =
        list.First |> List.unfold (fun i -> if i <> null then Some(i, i.Next) else None)

    for i in 1..times do
        originalOrder
        |> Seq.iter (fun n ->

            let amount =
                (if n.Value >= LanguagePrimitives.GenericZero then
                     n.Value
                 else
                     n.Value - LanguagePrimitives.GenericOne) % (len - LanguagePrimitives.GenericOne)

            if n.Value <> LanguagePrimitives.GenericZero then
                let targetNode = n |> walk (int amount)
                list.Remove n
                list.AddAfter(targetNode, n)
                ())

    list

let inline coordinates (shuffled: 'a LinkedList) =
    (shuffled.Find(LanguagePrimitives.GenericZero), [ 1..3 ])
    ||> List.scan (fun n _ -> n |> walk 1000)
    |> List.sumBy (fun n -> n.Value)

let part1 (input: int array) =
    shuffle 1 input.Length input |> coordinates

let part2 (input: int array) =
    let array = input |> Array.map (int64 >> ((*) 811589153L))
    array |> shuffle 10 array.LongLength |> coordinates

let run () =
    let input = System.IO.File.ReadAllLines "inputs/day20.txt" |> Array.map int
    $"Part1: {input |> part1} Part2: {input |> part2}"

module tests =
    open Xunit
    open Swensen.Unquote

    let example = [| 1; 2; -3; 3; -2; 0; 4 |]

    [<Fact>]
    let ``Part 1`` () = part1 example =! 3

    [<Fact>]
    let ``Part 2`` () = part2 example =! 1623178306
