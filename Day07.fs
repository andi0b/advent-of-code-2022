module aoc22.Day07

open FSharp.Text.RegexProvider

type Node =
    | Directory of name: string * items: Node Set ref
    | File of name: string * size: int

module Directory =
    let empty name = Directory(name, ref Set.empty)

module TreeBuilder =
    type CdRegex = Regex< @"\$ cd (?<directory>.*)" >
    type ListRegex = Regex< @"\$ ls" >
    type SizeRegex = Regex< @"(?<size>\d+) (?<name>\S+)" >
    type DirRegex = Regex< @"dir (?<name>\S+)" >

    let (|CdCmd|LsCmd|Output|Unknown|) line =
        [ CdRegex().TryTypedMatch >> Option.map (fun m -> CdCmd m.directory.Value)

          ListRegex().TryTypedMatch >> Option.map (fun m -> LsCmd)

          SizeRegex().TryTypedMatch
          >> Option.map (fun m ->
              let size = m.size.Value |> int
              let name = m.name.Value
              Output(File(name, size)))

          DirRegex().TryTypedMatch
          >> Option.map (fun m ->
              let name = m.name.Value
              Output(Directory.empty name)) ]
        |> List.fold (fun s n -> s |> Option.orElseWith (fun () -> n line)) None
        |> Option.defaultValue Unknown    
    
    type StateStack = (string * Node Set ref) list

    let pwdNodes (state: StateStack) = (state.Head |> snd).Value

    let addNode node (state: StateStack) =
        let nodes = state.Head |> snd
        nodes.Value <- nodes.Value |> Set.add node
        state

    let getDirectory name =
        Seq.tryFind (function
            | Directory(n, _) -> n = name
            | _ -> false)

    let pushd name (state: StateStack) =
        match (state |> pwdNodes |> getDirectory name) with
        | Some(Directory(name, nodes)) -> (name, nodes) :: state
        | _ -> failwith "directory doesn't exist"

    let popd (state: StateStack) = state.Tail

    let popdToRoot (state: StateStack) = [ state |> List.last ]

    let createTree (lines: string seq) =
        let folder state line =
            match line with
            | CdCmd dir ->
                match dir with
                | ".." -> state |> popd
                | "/" -> state |> popdToRoot
                | x -> state |> pushd x
            | Output node -> state |> addNode node
            | LsCmd -> state
            | Unknown -> failwith $"Unknown command: {line}"

        lines
        |> Seq.fold folder [ ("/", [] |> Set.ofList |> ref) ]
        |> Seq.last
        |> Directory

type SizeTreeNode =
    { name: string
      size: int
      children: SizeTreeNode list }

let rec sizeTree node =
    match node with
    | Directory(name, nodes) ->
        let subDirs = nodes.Value |> Seq.choose sizeTree |> Seq.toList
        let subDirSizes = subDirs |> Seq.sumBy (fun { size = size } -> size)

        let fileSizeFolder size =
            function
            | File(_, fileSize) -> size + fileSize
            | _ -> size

        let fileSizes = nodes.Value |> Seq.fold fileSizeFolder 0

        { name = name
          size = subDirSizes + fileSizes
          children = subDirs }
        |> Some
    | File _ -> None

let flatDirectorySizes node =
    let rec flatten (list: SizeTreeNode list) : (string * int) list =
        let toTuple item = (item.name, item.size)

        list
        |> List.collect (fun x ->
            match x.children with
            | [] -> [ x |> toTuple ]
            | children -> (x |> toTuple) :: (children |> flatten))

    sizeTree node |> Option.toList |> flatten

let part1 root =
    root
    |> flatDirectorySizes
    |> List.map snd
    |> List.filter (fun x -> x <= 100000)
    |> List.sum

let part2 root =
    let sizes = root |> flatDirectorySizes
    let totalUsed = sizes |> List.find (fst >> ((=) "/")) |> snd
    let needToDelete = totalUsed - 40_000_000

    sizes |> List.map snd |> List.filter (fun x -> x >= needToDelete) |> List.min

let run () =
    let input =
        System.IO.File.ReadAllLines("inputs/day07.txt") |> TreeBuilder.createTree

    $"Part 1: {input |> part1} Part2: {input |> part2}"

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
        [ "$ cd /"
          "$ ls"
          "dir a"
          "14848514 b.txt"
          "8504156 c.dat"
          "dir d"
          "$ cd a"
          "$ ls"
          "dir e"
          "29116 f"
          "2557 g"
          "62596 h.lst"
          "$ cd e"
          "$ ls"
          "584 i"
          "$ cd .."
          "$ cd .."
          "$ cd d"
          "$ ls"
          "4060174 j"
          "8033020 d.log"
          "5626152 d.ext"
          "7214296 k" ]
        |> TreeBuilder.createTree

    [<Fact>]
    let ``TreeBuilder with example`` () =
        let contents =
            [ Directory(
                  "a",
                  [ Directory("e", [ File("i", 584) ] |> Set.ofList |> ref)
                    File("f", 29116)
                    File("g", 2557)
                    File("h.lst", 62596) ]
                  |> Set.ofList
                  |> ref
              )
              File("b.txt", 14848514)
              File("c.dat", 8504156)
              Directory(
                  "d",
                  [ File("j", 4060174)
                    File("d.log", 8033020)
                    File("d.ext", 5626152)
                    File("k", 7214296) ]
                  |> Set.ofList
                  |> ref
              ) ]

        let expected = Directory("/", contents |> Set.ofList |> ref)
        example =! expected

    [<Fact>]
    let ``Part 1`` () = part1 example =! 95437

    [<Fact>]
    let ``Part 2`` () = part2 example =! 24933642
