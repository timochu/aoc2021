#time // puzzle: https://adventofcode.com/2021/day/10

let openings = Set "([{<"
let closings = Set ")]}>"
let scoreMap = Map [(')', 3) ; (']', 57) ; ('}', 1197) ; ('>', 25137)]

let score x = scoreMap.Item x

let openingCount = Seq.where openings.Contains >> Seq.length

let closingCount = Seq.where closings.Contains >> Seq.length

let isCompleteChunk x = openingCount x = closingCount x

let hasMismatchingBrackets x =
    let firstChar = List.head x
    let lastChar = List.last x
    let openingIndex = openings |> Seq.findIndex ((=) firstChar)
    let closingIndex = closings |> Seq.findIndex ((=) lastChar)
    openingIndex <> closingIndex

let isCorrupt x = isCompleteChunk x && hasMismatchingBrackets x

let chunkify (x : char list) =
    let rec chunk (chunks : char list list) (aggregate : char list) (line : char list) =
        if  openingCount line = 0 then chunks
        else
            let newAggregate = 
                match line |> List.tryItem aggregate.Length with
                | Some i -> aggregate @ [i]
                | None -> aggregate
            match aggregate.Length = newAggregate.Length || isCompleteChunk newAggregate with
            | true -> 
                let startFrom = line.Tail |> List.skipWhile closings.Contains
                if startFrom.IsEmpty then aggregate :: chunks
                else chunk (newAggregate :: chunks) [startFrom.Head] (startFrom)
            | false ->
                chunk chunks newAggregate line
    chunk [] [x.Head] x

// // Answer 1
"inputs/day10.txt" 
|> System.IO.File.ReadAllLines 
|> Array.map (Seq.toList >> chunkify)
|> Array.where (List.exists isCorrupt)
|> Array.map (List.where isCorrupt >> List.map List.last)
|> List.concat
|> List.map score
|> List.sum
|> printfn "%i"