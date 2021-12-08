#time // puzzle: https://adventofcode.com/2021/day/8

"inputs/day08.txt" 
|> System.IO.File.ReadAllLines 
|> Seq.map (fun (s:string) -> s.Split " | " |> Seq.last |> fun (x:string) -> x.Split " ") 
|> Seq.concat 
|> Seq.where (fun x -> x.Length < 5 || x.Length = 7)
|> Seq.length
|> printfn "%A"