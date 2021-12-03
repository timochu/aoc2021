// puzzle: https://adventofcode.com/2021/day/3
#time

let diagnostics = "inputs/day03.txt" |> System.IO.File.ReadAllLines 

// Answer 1
diagnostics 
|> Seq.transpose
|> Seq.map (Seq.countBy id >> Seq.maxBy snd >> fst) 
|> System.String.Concat
|> fun bits -> System.Convert.ToUInt16 (bits, 2)
|> fun gamma -> gamma * (~~~gamma <<< 4 >>> 4)
|> printfn "answer 1: %i"

// Answer 2
let rec rating i comparator diagnostics  =
    match diagnostics with
    | [| result |] -> result |> fun bits -> System.Convert.ToInt32 (bits, 2)
    | _ -> 
        let common = diagnostics |> Seq.transpose |> Seq.item i |> Seq.countBy id |> comparator |> fst
        diagnostics |> Array.where (fun bits -> bits.[i] = common) |> rating (i+1) comparator

let oxygen = diagnostics |> rating 0 (Seq.sortByDescending fst >> Seq.maxBy snd)
let co2 = diagnostics |> rating 0 (Seq.sortBy fst >> Seq.minBy snd )

printfn "answer 2: %i" (oxygen * co2)