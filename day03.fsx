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
type Criteria = Oxygen | CarbonDioxide

let rec rating i items criteria =
    match items with
    | [| result |] -> System.Convert.ToInt32 (result, 2)
    | _ ->
        let bitSelector = 
            match criteria with
            | Oxygen -> Seq.sortByDescending fst >> Seq.maxBy snd
            | CarbonDioxide -> Seq.sortBy fst >> Seq.minBy snd
        let bit = items |> Seq.transpose |> Seq.item i |> Seq.countBy id |> bitSelector |> fst
        let remaining = items |> Array.where (fun bits -> bits.[i] = bit)
        rating (i+1) remaining criteria

[Oxygen; CarbonDioxide] |> Seq.map (rating 0 diagnostics) |> Seq.reduce (*) |> printfn "%i"