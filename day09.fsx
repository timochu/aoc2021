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

let getAdjacentValues (i,_) = 
    [| if i % width <> 0 then yield heightmap[i-1]
       if i % width <> width - 1 then yield heightmap[i+1]
       if i - width >= 0 then yield heightmap[i-width]
       if i + width < length then yield heightmap[i+width] |]

let isLowerThanAdjacent (i, p) =
    (i, p) |> getAdjacentValues |> Array.forall (fun (_, ap) -> p < ap) 

let lowPoints = heightmap |> Array.where isLowerThanAdjacent 

let getBasinSize point =
    let rec expander points length =
        points
        |> Array.collect getAdjacentValues
        |> Array.where (fun (_,p) -> p < 9 )
        |> Array.append points
        |> Array.distinctBy fst
        |> function 
        | basin when basin.Length = length -> length
        | basin -> expander basin basin.Length
    expander [|point|] 0

// Answer 1
lowPoints |> Array.sumBy (snd >> (+) 1) |> printfn "%i"

// Answer 2
lowPoints
|> Array.map getBasinSize
|> Array.sortDescending
|> Array.take 3
|> Array.reduce (*)
|> printfn "%i"