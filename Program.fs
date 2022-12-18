open System.Threading.Tasks
open aoc22

let skip = (fun () -> "skipped")

let days =
    [ Day01.run
      Day02.run
      Day03.run
      Day04.run
      Day05.run
      Day06.run
      Day07.run
      Day08.run
      Day09.run
      Day10.run
      Day11.run
      Day12.run
      skip
      skip
      skip
      skip
      skip
      Day18.run ]

let tasks = days |> List.map Task.Run

task {
    for i, task in tasks |> List.indexed do
        let! result = task
        printfn $"Day {i + 1} {result}"
}
|> Task.WaitAll
