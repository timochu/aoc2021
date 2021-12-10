#time // puzzle: https://adventofcode.com/2021/day/10

let openings = Set "([{<"
let closings = Set ")]}>"
let scores1 = Map [(')', 3) ; (']', 57) ; ('}', 1197) ; ('>', 25137)] 
let scores2 = Map [(')', 1L) ; (']', 2L) ; ('}', 3L) ; ('>', 4L)] 

let getClosing char = closings |> Seq.item (openings |> Seq.findIndex ((=) char))

let openingCount = Seq.where openings.Contains >> Seq.length

let closingCount = Seq.where closings.Contains >> Seq.length

let isCompleteChunk chunk = openingCount chunk = closingCount chunk

let hasMismatchingBrackets chunk =
    ( openings |> Seq.findIndex ((=) (List.head chunk)),
      closings |> Seq.findIndex ((=) (List.last chunk)) ) 
      ||> (<>)

let isCorrupt x = isCompleteChunk x && hasMismatchingBrackets x

let toChunks line =
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
                if startFrom.IsEmpty then newAggregate :: chunks
                else chunk (newAggregate::chunks) [startFrom.Head] startFrom
            | false ->
                chunk chunks newAggregate line
    chunk [] [Seq.head line] (line |> Seq.toList)

let calculateAutocompleteScore chunks =
    chunks 
    |> List.where (isCompleteChunk >> not) 
    |> List.map (List.head >> getClosing)
    |> List.map (fun x -> scores2.Item x)
    |> List.fold (fun acc s -> (acc * 5L) + s) 0L

let chunks = "inputs/day10test.txt" |> System.IO.File.ReadAllLines |> Array.map toChunks

// // Answer 1
chunks
|> Array.where (List.exists isCorrupt)
|> Array.map (List.where isCorrupt >> List.map List.last)
|> List.concat
|> List.map (fun x -> scores1.Item x)
|> List.sum
|> printfn "%i"

// // Answer 2
chunks
|> Array.where (List.exists isCorrupt >> not)
|> Array.map calculateAutocompleteScore
|> Array.sort
|> fun scores -> scores[scores.Length / 2]
|> printfn "%A"