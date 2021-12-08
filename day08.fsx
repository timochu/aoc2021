#time // puzzle: https://adventofcode.com/2021/day/8

// Answer 1
"inputs/day08.txt" 
|> System.IO.File.ReadAllLines 
|> Seq.map (fun (s:string) -> s.Split " | " |> Seq.last |> fun (x:string) -> x.Split " ") 
|> Seq.concat 
|> Seq.where (fun x -> x.Length < 5 || x.Length = 7)
|> Seq.length
|> printfn "%A"

// Answer 2
let getOutputValue d =
    let d1 = d |> Seq.where (Seq.length >> (=) 2) |> Seq.head
    let d4 = d |> Seq.where (Seq.length >> (=) 4) |> Seq.head
    let d7 = d |> Seq.where (Seq.length >> (=) 3) |> Seq.head
    let d8 = d |> Seq.where (Seq.length >> (=) 7) |> Seq.head
    let d3 = d |> Seq.where (Seq.length >> (=) 5) |> Seq.where (Set.isSubset d1) |> Seq.head
    let d9 = d |> Seq.where (Seq.length >> (=) 6) |> Seq.where (Set.isSubset d3) |> Seq.head
    let d5 = d |> Seq.where (Seq.length >> (=) 5) |> Seq.except [d3]  |> Seq.where (Set.isSuperset d9) |> Seq.head
    let d2 = d |> Seq.where (Seq.length >> (=) 5) |> Seq.except [d5;d3] |>  Seq.head
    let d6 = d |> Seq.where (Seq.length >> (=) 6) |> Seq.except [d9] |> Seq.where (Set.isSubset d5) |> Seq.head
    let d0 = d |> Seq.except [d1;d2;d3;d4;d5;d6;d7;d8;d9] |> Seq.head
    let mapper y = [d0;d1;d2;d3;d4;d5;d6;d7;d8;d9] |> Seq.findIndex ((=) y) |> string
    d |> Seq.skip 10 |> Seq.map mapper |> Seq.reduce (+) |> int

"inputs/day08.txt" 
|> System.IO.File.ReadAllLines 
|> Seq.map (fun (s:string) -> s.Split([|" | "; " "|], System.StringSplitOptions.RemoveEmptyEntries))
|> Seq.map (fun (d : string array) -> d |> Seq.map Set)
|> Seq.map getOutputValue
|> Seq.sum
|> printfn "%A" 