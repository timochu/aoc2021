#time // puzzle: https://adventofcode.com/2021/day/4

type Board =  { All: int array ; Lines: int array array }

let toBoard (s : string) = 
    s.Split ([|' '; '\n'|], System.StringSplitOptions.RemoveEmptyEntries) 
    |> Array.map int 
    |> fun all -> 
        let horizontal = all |> Array.chunkBySize 5
        let vertical = horizontal |> Array.transpose
        { All = all ; Lines = horizontal |> Array.append vertical }

let isWinner drawn board = board.Lines |> Array.exists (Array.forall (fun number -> drawn |> Array.contains number ))

let calculateScore drawn board = board.All |> Array.except drawn |> Array.sum |> ( * ) (Array.last drawn)

let rec score drawn scores numbers boards =
    match boards |> Array.isEmpty with
    | true -> scores
    | false ->
        let drawn = numbers |> Array.take (drawn |> Array.length |> (+) 1)
        let winners = boards |> Array.where (isWinner drawn)
        let scores = scores |> Array.append (winners |> Array.map (calculateScore drawn))
        let remaining = boards |> Array.except winners
        score drawn scores numbers remaining

let boards = "inputs/day04.txt" |> System.IO.File.ReadAllText |> fun s -> s.Split "\n\n" |> Array.skip 1 |> Array.map toBoard
let numbers = "inputs/day04.txt" |> System.IO.File.ReadLines |> Seq.head |> fun s -> s.Split ',' |> Array.map int

score Array.empty Array.empty numbers boards |> fun scores -> printfn "%i\n%i" (Array.last scores) (Array.head scores)