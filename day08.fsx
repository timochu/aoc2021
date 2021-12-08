#time // puzzle: https://adventofcode.com/2021/day/8
open type System.String

let split (separator:string array) (s:string) = s.Split (separator, System.StringSplitOptions.RemoveEmptyEntries)

// Answer 1
"inputs/day08.txt" 
|> System.IO.File.ReadAllLines 
|> Seq.map (fun (s:string) -> s.Split " | " |> Seq.last |> fun (x:string) -> x.Split " ") 
|> Seq.concat 
|> Seq.where (fun x -> x.Length < 5 || x.Length = 7)
|> Seq.length
|> printfn "%A"

// Answer 2
let getOutputValue (digits : string array) =
    let d = digits |> Seq.map Seq.sort |> Seq.map Concat |> Seq.distinct
    let one     = d |> Seq.where (Seq.length >> (=) 2) |> Seq.head
    let four    = d |> Seq.where (Seq.length >> (=) 4) |> Seq.head
    let seven   = d |> Seq.where (Seq.length >> (=) 3) |> Seq.head
    let eight   = d |> Seq.where (Seq.length >> (=) 7) |> Seq.head
    let three   = d |> Seq.where (Seq.length >> (=) 5) |> Seq.where (fun s -> one |> Seq.except s |> Seq.isEmpty) |> Seq.head
    let nine    = d |> Seq.where (Seq.length >> (=) 6) |> Seq.where (fun s -> three |> Seq.except s |> Seq.isEmpty) |> Seq.head
    let five    = d |> Seq.except [three] |> Seq.where (Seq.length >> (=) 5) |> Seq.where (fun x -> x |> Seq.except nine |> Seq.isEmpty) |> Seq.head
    let two     = d |> Seq.except [five;three] |> Seq.where (Seq.length >> (=) 5) |>  Seq.head
    let six     = d |> Seq.except [nine] |> Seq.where (Seq.length >> (=) 6) |> Seq.where (fun x -> five |> Seq.except x |> Seq.isEmpty) |> Seq.head
    let zero    = d |> Seq.except [one;two;three;four;five;six;seven;eight;nine] |> Seq.head
    let mapper = Map [ (zero, '0'); (one, '1'); (two, '2'); (three, '3'); (four, '4'); (five, '5'); (six, '6'); (seven, '7'); (eight, '8'); (nine, '9') ]
    digits |> Seq.skip 10 |> Seq.map (Seq.sort >> Concat >> fun x -> mapper.[x]) |> Concat |> int

"inputs/day08.txt" 
|> System.IO.File.ReadAllLines 
|> Seq.map (fun (s:string) -> s.Split([|" | "; " "|], System.StringSplitOptions.RemoveEmptyEntries))
|> Seq.map getOutputValue
|> Seq.sum
|> printfn "%A" 