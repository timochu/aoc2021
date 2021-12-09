#time // puzzle: https://adventofcode.com/2021/day/9

let file = "inputs/day09.txt"

let heightmap = 
    file
    |> System.IO.File.ReadAllLines
    |> Array.map (Seq.map (string >> int) >> Seq.toArray)
    |> Array.concat
    |> Array.indexed

let width = file |> System.IO.File.ReadLines |> Seq.head |> Seq.length
let length = Array.length heightmap

let getAdjacentValues i = 
    [| if i % width <> 0 then yield heightmap[i-1]
       if i % width <> width - 1 then yield heightmap[i+1]
       if i - width >= 0 then yield heightmap[i-width]
       if i + width < length then yield heightmap[i+width] |]

let isLowerThanAdjacent (index, value) = 
    index |> getAdjacentValues |> Array.minBy snd |> snd |> (<) value

let lowPoints = heightmap |> Array.where isLowerThanAdjacent 

// Answer 1
lowPoints |> Array.sumBy (snd >> (+) 1) |> printfn "%i"

// Answer 2
let rec getBasinSize points length =
    let basin = 
        points
        |> Array.collect (fst >> getAdjacentValues) 
        |> Array.where (fun (_,p) -> p < 9 ) 
        |> Array.append points 
        |> Array.distinctBy fst
    match basin.Length = length with 
    | true -> length
    | false -> getBasinSize basin basin.Length

lowPoints 
|> Array.map (fun p -> getBasinSize [|p|] 0)
|> Array.sortDescending 
|> Array.take 3 
|> Array.reduce (*) 
|> printfn "%i"