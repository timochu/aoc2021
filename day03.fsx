// puzzle: https://adventofcode.com/2021/day/3
#time

let rotate d = d |> Seq.head |> Seq.mapi (fun i _ -> d |> Seq.map (Seq.item i))
let mostFrequent = Seq.countBy id >> Seq.maxBy snd >> fst

let diagnostics = "inputs/day03.txt" |> System.IO.File.ReadAllLines |> rotate

let gamma = diagnostics |> Seq.map mostFrequent |> Seq.toArray |> System.String |> fun b -> System.Convert.ToUInt16 (b, 2)
let epsilon = ~~~gamma <<< 4 >>> 4 

// Answer 1
printfn "%i" (gamma * epsilon)
