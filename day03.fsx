#time // puzzle: https://adventofcode.com/2021/day/3

let diagnostics = "inputs/day03.txt" |> System.IO.File.ReadAllLines 

// Answer 1
diagnostics 
|> Seq.transpose
|> Seq.map (Seq.countBy id >> Seq.maxBy snd >> fst) 
|> System.String.Concat
|> fun bits -> System.Convert.ToUInt16 (bits, 2)
|> fun gamma -> gamma * (~~~gamma <<< 4 >>> 4)
|> printfn "%i"

// Answer 2
let rec getRating i bitCriteria items  =
    match items with
    | [| result |] -> 
        result |> fun bits -> System.Convert.ToInt32 (bits, 2)
    | remaining ->
        let bit = remaining |> Seq.transpose |> Seq.item i |> Seq.countBy id |> bitCriteria
        remaining |> Array.where (fun bits -> bits.[i] = bit) |> getRating (i+1) bitCriteria

let oxygen = diagnostics |> getRating 0 (Seq.sortByDescending fst >> Seq.maxBy snd >> fst)
let co2 = diagnostics |> getRating 0 (Seq.sortBy fst >> Seq.minBy snd >> fst)
printfn "%i" (oxygen * co2)