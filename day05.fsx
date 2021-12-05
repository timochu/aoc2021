#time // puzzle: https://adventofcode.com/2021/day/5

let toLine (s : string) =
    s.Split " -> " 
    |> Array.map (fun (x:string) -> x.Split ',')
    |> Array.concat
    |> Array.map int
    |> fun p -> (p[0], p[1]) , (p[2], p[3])

let isHorizontalOrVertical ((x1 , y1) , (x2 , y2)) = 
    x1 = x2 || y1 = y2

let getAllPoints ((x1 , y1) , (x2 , y2)) =
    match isHorizontalOrVertical ((x1,y1),(x2,y2)) with
    | false ->
        let xIncrement = if x1>x2 then -1 else 1
        let yIncrement = if y1>y2 then -1 else 1
        let xs = [| x1 .. xIncrement .. x2 |]
        let ys = [| y1 .. yIncrement .. y2 |]
        [| 0 .. Array.length xs - 1 |] |> Array.map (fun i -> xs[i], ys[i])
    | true -> 
        let (xUpper, xLower) = if x1>x2 then x1,x2 else x2,x1
        let (yUpper, yLower) = if y1>y2 then y1,y2 else y2,y1
        [|
        for x in xLower .. xUpper do
            for y in yLower .. yUpper ->
                x, y
        |]

let overlapping = 
    Array.map getAllPoints
    >> Array.concat
    >> Array.countBy id
    >> Array.where (fun (_, count) -> count > 1)
    >> Array.length
    >> printfn "%A"

let lines = "inputs/day05.txt" |> System.IO.File.ReadAllLines |> Array.map toLine

// // Answer 1
lines |> Array.where isHorizontalOrVertical |> overlapping

// // Answer 2
lines |> overlapping
