#time // puzzle: https://adventofcode.com/2021/day/6

let fish = 
    let count num = Seq.where ((=) num) >> Seq.length >> int64
    "inputs/day06.txt" |> System.IO.File.ReadAllText |> fun (s:string) -> s.Split ',' 
    |> fun x -> [ 0L; count "1" x; count "2" x; count "3" x; count "4" x; count "5" x; 0L; 0L; 0L ]

let rec count days f =
    if days = 0 then f |> Seq.sum
    else count (days-1) [ f[1]; f[2]; f[3]; f[4]; f[5]; f[6]; f[7] + f[0]; f[8]; f[0] ]

// Answer 1
fish |> count 80 |> printfn "%i"

// Answer 2
fish |> count 256 |> printfn "%i"