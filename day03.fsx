// puzzle: https://adventofcode.com/2021/day/3
#time

let diagnostics = "inputs/day03.txt" |> System.IO.File.ReadAllLines 

// Answer 1
diagnostics 
|> Seq.transpose
|> Seq.map (Seq.countBy id >> Seq.maxBy snd >> fst) 
|> Seq.toArray |> System.String 
|> (fun s -> (System.Convert.ToUInt16 (s, 2))) 
|> (fun i -> i * (~~~i <<< 4 >>> 4))
|> printfn "%i"