#time // puzzle: https://adventofcode.com/2021/day/6

let fish = "inputs/day06.txt" |> System.IO.File.ReadAllText |> fun (s:string) -> s.Split ',' |> Array.map int

let rec counter i days fish =
    if i = days then fish |> Array.length
    else
        let spawn = fish |> Array.where ((=) 0) |> Array.length |> (fun x -> Array.replicate x 8)
        let fishes = fish |> Array.map (fun x -> x-1) |> Array.map (fun x -> if x < 0 then 6 else x) |> Array.append spawn
        counter (i+1) days fishes

// Answer 1
counter 0 80 fish |> printfn "%i"

// Answer 2
// counter 0 256 fish |> printfn "%i"