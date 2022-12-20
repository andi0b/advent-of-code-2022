[<AutoOpen>]
module aoc22.Prelude

open System.Text.RegularExpressions

let tryRegex pattern input =
    let m = Regex.Match(input, pattern)

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

let (|Regex|_|) pattern input = tryRegex pattern input

let tryRegexG pattern input =
    let m = Regex.Matches(input, pattern)

    if m.Count > 0 then
        Some([ for x in m -> x.Value ])
    else
        None

let (|RegexG|_|) pattern input = tryRegexG pattern input

let tryPrefix (p: string) (s: string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let (|Prefix|_|) (p: string) (s: string) = tryPrefix p s
