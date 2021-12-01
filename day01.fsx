// puzzle: https://adventofcode.com/2021/day/1
#time

let input = System.IO.File.ReadAllLines "inputs/day01.txt" |> Seq.map int
let deeper (x,y) = x < y 

// Answer 1
Seq.pairwise input
|> Seq.where deeper
|> Seq.length
|> printfn "%i"

// Answer 2
Seq.windowed 3 input
|> Seq.map Seq.sum
|> Seq.pairwise
|> Seq.where deeper
|> Seq.length
|> printfn "%i"
