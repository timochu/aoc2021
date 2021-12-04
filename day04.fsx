#time // puzzle: https://adventofcode.com/2021/day/4

type Board =  { All: int array ; Lines: int array array }

let toBoard (s : string) = 
    s.Split ([|' '; '\n'|], System.StringSplitOptions.RemoveEmptyEntries) 
    |> Array.map int 
    |> fun all -> 
        let horizontal = all |> Array.chunkBySize 5
        let vertical = horizontal |> Array.transpose
        { All = all ; Lines = horizontal |> Array.append vertical }

let isWinner drawn board = board.Lines |> Array.exists (fun line -> line |> Array.forall(fun number -> drawn |> Array.contains number))

let calculateScore drawn board = board.All |> Array.except drawn |> Array.sum |> ( * ) (Array.last drawn)

let rec score iteration scores numbers boards =
    match iteration = Array.length numbers with
    | true -> scores
    | false ->
        let drawn = numbers |> Array.take iteration
        let winners = boards |> Array.where (isWinner drawn)
        let scores = scores |> Array.append (winners |> Array.map (calculateScore drawn))
        let remaining = boards |> Array.except winners
        score (iteration+1) scores numbers remaining

let boards = "inputs/day04.txt" |> System.IO.File.ReadAllText |> fun s -> s.Split "\n\n" |> Array.skip 1 |> Array.map toBoard
let numbers = "inputs/day04.txt" |> System.IO.File.ReadLines |> Seq.head |> fun s -> s.Split ',' |> Array.map int

// Answer 1 & 2
(numbers, boards) ||> score 0 Array.empty |> fun scores -> printfn "%i\n%i" (Array.last scores) (Array.head scores)