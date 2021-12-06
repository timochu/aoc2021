#time // puzzle: https://adventofcode.com/2021/day/6

let c num = Seq.where ((=) num) >> Seq.length >> int64
let fish = "inputs/day06.txt" |> System.IO.File.ReadAllText |> fun (s:string) -> s.Split ',' |> fun f -> [ 0L; c "1" f; c "2" f; c "3" f; c "4" f; c "5" f; 0L; 0L; 0L ]

let rec count f days =
    if days = 0 then f |> Seq.sum
    else count [ f[1]; f[2]; f[3]; f[4]; f[5]; f[6]; f[7] + f[0]; f[8]; f[0] ] (days-1)

// Answer 1 & 2
[ 80; 256 ] |> Seq.iter (count fish >> printfn "%i")