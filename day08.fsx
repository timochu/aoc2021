#time // puzzle: https://adventofcode.com/2021/day/8

// Answer 1
"inputs/day08.txt" 
|> System.IO.File.ReadAllLines 
|> Seq.map (fun (s:string) -> s.Split " | " |> Seq.last |> fun (x:string) -> x.Split " ") 
|> Seq.concat 
|> Seq.where (fun x -> x.Length < 5 || x.Length = 7)
|> Seq.length
|> printfn "%A"

// Answer 2
let getOutputValue d =
    let (&&.) f g x = f x && g x
    let length l s = Seq.length s = l
    let d1 = d |> Seq.find (length 2)
    let d4 = d |> Seq.find (length 4)
    let d7 = d |> Seq.find (length 3) 
    let d8 = d |> Seq.find (length 7)
    let d3 = d |> Seq.find (length 5 &&. Set.isSubset d1)
    let d9 = d |> Seq.find (length 6 &&. Set.isSubset d3)
    let d5 = d |> Seq.except [d3] |> Seq.find (length 5 &&. Set.isSuperset d9)
    let d2 = d |> Seq.except [d5;d3] |> Seq.find (length 5)
    let d6 = d |> Seq.except [d9] |> Seq.find (length 6 &&. Set.isSubset d5)
    let d0 = d |> Seq.except [d1;d2;d3;d4;d5;d6;d7;d8;d9] |> Seq.head
    let mapper y = [d0;d1;d2;d3;d4;d5;d6;d7;d8;d9] |> Seq.findIndex ((=) y) |> string
    d |> Seq.skip 10 |> Seq.map mapper |> Seq.reduce (+) |> int

"inputs/day08.txt" 
|> System.IO.File.ReadAllLines 
|> Seq.map (fun (s:string) -> s.Split([|" | "; " "|], System.StringSplitOptions.RemoveEmptyEntries))
|> Seq.map (Seq.map Set >> getOutputValue)
|> Seq.sum
|> printfn "%A" 