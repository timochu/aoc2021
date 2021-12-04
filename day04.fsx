#time // puzzle: https://adventofcode.com/2021/day/4

type Board = 
    { All: int seq
      Horizontal: int seq seq
      Vertical: int seq seq }

let toBoard (s : string) = 
    s.Split ([|' '; '\n'|], System.StringSplitOptions.RemoveEmptyEntries) 
    |> Seq.map int 
    |> (fun all -> 
        let horizontal = all |> Seq.chunkBySize 5 |> Seq.map Array.toSeq
        let vertical = horizontal |> Seq.transpose
        { All = all ; Horizontal = horizontal ; Vertical = vertical } )

let getAnswer drawn board =
    let checkWin = Seq.exists (fun line -> line |> Seq.forall(fun number -> drawn |> Seq.contains number))
    match checkWin board.Horizontal, checkWin board.Vertical with
    | false, false -> None
    | _, _ -> board.All |> Seq.except drawn |> Seq.sum |> (*) (Seq.last drawn) |> Some

let rec score iteration numbers answer boards =
    match answer with
    | Some score -> score
    | None ->
        let drawn = numbers |> Seq.take iteration
        let a = boards |> Seq.tryPick (fun x -> getAnswer drawn x)
        score (iteration+1) numbers a boards

let boards = "inputs/day04.txt" |> System.IO.File.ReadAllText |> fun s -> s.Split "\n\n" |> Seq.skip 1 |> Seq.map toBoard
let numbers = "inputs/day04.txt" |> System.IO.File.ReadLines |> Seq.head |> fun s -> s.Split ',' |> Seq.map int

// Answer 1
score 5 numbers None boards |> printfn "%i"
