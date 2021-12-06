#time // puzzle: https://adventofcode.com/2021/day/6

let n num = Seq.where ((=) num) >> Seq.length >> uint64
let fish = "inputs/day06.txt" |> System.IO.File.ReadAllText |> fun (s:string) -> s.Split ',' |> Seq.map int |> fun x -> [| n 0 x ; n 1 x ; n 2 x ; n 3 x; n 4 x; n 5 x; n 6 x; n 7 x ; n 8 x |]

let rec count days fish =
    if days = 0 then fish |> Seq.sum
    else count (days-1) [| fish[1]; fish[2]; fish[3]; fish[4]; fish[5]; fish[6]; fish[7] + fish[0]; fish[8]; fish[0] |]

// Answer 1
fish |> count 80 |> printfn "%i"

// Answer 2
fish |> count 256 |> printfn "%i"