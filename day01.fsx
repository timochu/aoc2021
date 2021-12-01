// puzzle: https://adventofcode.com/2021/day/1
#time

let input = System.IO.File.ReadAllLines "inputs/day01.txt" |> Seq.map int

// Answer 1
Seq.pairwise input |> Seq.filter (fun (x,y) -> x < y) |> Seq.length |> printfn "%i"

// Answer 2
Seq.windowed 3 input |> Seq.map Seq.sum |> Seq.pairwise |> Seq.filter (fun (x,y) -> x < y) |> Seq.length |> printfn "%i"
