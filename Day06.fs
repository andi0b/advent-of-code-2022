module aoc22.Day06

let isDistinct array = array |> Array.distinct = array

let findMarker length =
    Seq.windowed length >> Seq.findIndex isDistinct >> (+) length

let part1 = findMarker 4
let part2 = findMarker 14

let run () =
    let input = System.IO.File.ReadAllLines("inputs/day06.txt").[0]
    $"Part 1: {input |> part1} Part2: {input |> part2}"

module tests =
    open Swensen.Unquote
    open Xunit
    
    let examples =
        [ "mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7, 19
          "bvwbjplbgvbhsrlpgdmjqwftvncz", 5, 23
          "nppdvjthqldpwncqszvftbrmjlhg", 6, 23
          "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10, 29
          "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11, 26 ]

    let part1MemberData =
        examples
        |> Seq.map (fun (input, expected, _) -> [| input :> obj; expected :> obj |])

    [<Theory>]
    [<MemberData(nameof part1MemberData)>]
    let part1 (input, expected) = test <@ part1 input = expected @>

    let part2MemberData =
        examples
        |> Seq.map (fun (input, _, expected) -> [| input :> obj; expected :> obj |])

    [<Theory>]
    [<MemberData(nameof part2MemberData)>]
    let part2 (input, expected) = test <@ part2 input = expected @>
