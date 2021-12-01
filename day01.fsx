// puzzle: https://adventofcode.com/2021/day/1
#time

let input = System.IO.File.ReadAllLines "inputs/day01.txt" |> Seq.map int
let deeper (x,y) = x < y 

// Answer 1
input |> Seq.pairwise |> Seq.where deeper |> Seq.length |> printfn "%i"

// Answer 2
input |> Seq.windowed 3 |> Seq.map Seq.sum |> Seq.pairwise |> Seq.where deeper |> Seq.length |> printfn "%i"
