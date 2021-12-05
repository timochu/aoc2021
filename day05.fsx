#time // puzzle: https://adventofcode.com/2021/day/5

let toLine (s : string) =
    s.Split " -> " 
    |> Array.map (fun (x:string) -> x.Split ',')
    |> Array.concat
    |> Array.map int
    |> fun p -> (p[0], p[2]) , (p[1], p[3])

let isHorizontalOrVertical (line : (int * int) * (int * int)) =  
    line |> fst ||> (=) || line |> snd ||> (=)
    
let getAllPoints (line : (int * int) * (int * int)) =
    match line with
    | (x1,x2),(y1,y2) when x1 = x2 ->
        if y1 > y2 then [| for i in y2 .. y1 -> (x1, i) |]
        else            [| for i in y1 .. y2 -> (x1, i) |]
    | (x1,x2),(y1,y2) when y1 = y2 ->
        if x1 > x2 then [| for i in x2 .. x1 -> (i, y1) |]
        else            [| for i in x1 .. x2 -> (i, y1) |]
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
