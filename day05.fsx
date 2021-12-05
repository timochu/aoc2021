#time // puzzle: https://adventofcode.com/2021/day/5

let toLine (s : string) =
    s.Split " -> " 
    |> Array.map (fun (x:string) -> x.Split ',')
    |> Array.concat
    |> Array.map int
    |> fun p -> (p[0], p[1]) , (p[2], p[3])

let isHorizontalOrVertical ((x1 , y1) , (x2 , y2)) = x1 = x2 || y1 = y2
    
let getAllPoints ((x1 , y1) , (x2 , y2)) =
    match x1 = x2, y1 = y2, y1 > y2, x1 > x2 with
    | true, _, true, _  -> [| for i in y2 .. y1 -> (x1, i) |]
    | true, _, false, _ -> [| for i in y1 .. y2 -> (x1, i) |]
    | _, true, _, true  -> [| for i in x2 .. x1 -> (i, y1) |]
    | _, true, _, false -> [| for i in x1 .. x2 -> (i, y1) |]
    | _ -> failwith "there was a diagonal line amongst the lines!"
    
// Answer 1
"inputs/day05.txt" 
|> System.IO.File.ReadAllLines 
|> Array.map toLine 
|> Array.where isHorizontalOrVertical 
|> Array.map getAllPoints 
|> Array.concat
|> Array.countBy id
|> Array.where (fun (_, count) -> count > 1)
|> Array.length
|> printfn "%A"
