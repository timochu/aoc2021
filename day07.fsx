#time // puzzle: https://adventofcode.com/2021/day/6

let positions = "inputs/day07.txt" |> System.IO.File.ReadAllText |> fun (s:string) -> s.Split ',' |> Seq.map int

// Answer 1
[ positions |> Seq.min .. positions |> Seq.max ]
|> Seq.map (fun r -> positions |> Seq.map(fun pos -> r-pos |> abs) |> Seq.sum)  
|> Seq.min
|> printfn "%A"

// Answer 2
[ positions |> Seq.min .. positions |> Seq.max ]
|> Seq.map (fun r -> positions |> Seq.map(fun pos -> [1 .. (r-pos |> abs)] |> Seq.sum) |> Seq.sum)  
|> Seq.min
|> printfn "%A"