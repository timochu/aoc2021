// puzzle: https://adventofcode.com/2021/day/1
#time

let depths = "inputs/day01.txt" |> System.IO.File.ReadAllLines |> Seq.map int
let deeper (x,y) = x < y 

// Answer 1
depths |> Seq.pairwise |> Seq.where deeper |> Seq.length |> printfn "%i"

// Answer 2
depths |> Seq.windowed 3 |> Seq.map Seq.sum |> Seq.pairwise |> Seq.where deeper |> Seq.length |> printfn "%i"
