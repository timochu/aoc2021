#time // puzzle: https://adventofcode.com/2021/day/6

let n num = Seq.where ((=) num) >> Seq.length >> uint64
let fish = "inputs/day06.txt" |> System.IO.File.ReadAllText |> fun (s:string) -> s.Split ',' |> fun x -> [| 0UL; n "1" x; n "2" x; n "3" x; n "4" x; n "5" x; 0UL; 0UL; 0UL |]

let rec count days fish =
    if days = 0 then fish |> Seq.sum
    else count (days-1) [| fish[1]; fish[2]; fish[3]; fish[4]; fish[5]; fish[6]; fish[7] + fish[0]; fish[8]; fish[0] |]

// Answer 1
fish |> count 80 |> printfn "%i"

// Answer 2
fish |> count 256 |> printfn "%i"