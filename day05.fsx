#time // puzzle: https://adventofcode.com/2021/day/5

let toLine (s : string) =
    s.Split ([|" -> "; ","|], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int
    |> fun p -> p[0], p[1], p[2], p[3]

let isHorizontalOrVertical (x1, y1, x2 , y2) = 
    x1 = x2 || y1 = y2

let getAllPoints (x1, y1, x2, y2) =
    let step a b = if a > b then -1 else 1
    let xs = [| x1 .. step x1 x2 .. x2 |]
    let ys = [| y1 .. step y1 y2 .. y2 |]
    if isHorizontalOrVertical (x1, y1, x2, y2) then
        Array.allPairs xs ys
    else 
        Array.zip xs ys 

let overlapping = 
    Array.map getAllPoints
    >> Array.concat
    >> Array.countBy id
    >> Array.where (fun (_, count) -> count > 1)
    >> Array.length
    >> printfn "%A"

let lines = "inputs/day05.txt" |> System.IO.File.ReadAllLines |> Array.map toLine

// Answer 1
lines |> Array.where isHorizontalOrVertical |> overlapping

// Answer 2
lines |> overlapping