#time // puzzle: https://adventofcode.com/2021/day/9
let heightmap = 
    "inputs/day09.txt" 
    |> System.IO.File.ReadAllLines
    |> Array.map (Seq.map (string >> int) >> Seq.toArray)
    |> Array.concat

let width = 100
let length = Array.length heightmap

let getAdjacentValues i = 
    seq { if i % width <> 0 then yield heightmap[i-1] 
          if i % width <> width - 1 then yield heightmap[i+1]
          if i - width >= 0 then yield heightmap[i-width]
          if i + width < length then yield heightmap[i+width] }

let isLowerThanAdjacent (index, value) = 
    index |> getAdjacentValues |> Seq.min |> (<) value

heightmap 
|> Array.indexed
|> Array.where isLowerThanAdjacent 
|> Array.sumBy (snd >> (+) 1 )
|> printfn "%i"