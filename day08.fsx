#time // puzzle: https://adventofcode.com/2021/day/8
open type System.String

let split (separator:string array) (s:string) = s.Split (separator, System.StringSplitOptions.RemoveEmptyEntries)

// Answer 1
"inputs/day08.txt" 
|> System.IO.File.ReadAllLines 
|> Seq.map (split [|" | "|] >> Seq.last >> split [|" "|]) 
|> Seq.concat 
|> Seq.where (fun x -> x.Length < 5 || x.Length = 7)
|> Seq.length
|> printfn "%A"

// Answer 2
let decipherSegments (digits : string array) =
    let d = digits |> Array.map Seq.sort |> Array.map Concat |> Array.distinct |> Array.map Seq.toArray
    let one = d |> Seq.where (Seq.length >> (=) 2) |> Seq.exactlyOne
    let four = d |> Seq.where (Seq.length >> (=) 4) |> Seq.exactlyOne
    let seven = d |> Seq.where (Seq.length >> (=) 3) |> Seq.exactlyOne
    let eight = d |> Seq.where (Seq.length >> (=) 7) |> Seq.exactlyOne
    let three = d |> Seq.where (fun s -> s |> Seq.length = 5 && one |> Seq.forall (fun x -> s |> Seq.exists ((=) x))) |> Seq.exactlyOne
    let nine = d |> Seq.where (fun s -> s |> Seq.length = 6 && three |> Seq.forall (fun x -> s |> Seq.exists ((=) x))) |> Seq.exactlyOne
    let five = d |> Seq.except [ three ] |> Seq.where (Seq.length >> (=) 5) |> Seq.where (fun x -> x |> Seq.except nine |> Seq.isEmpty) |> Seq.exactlyOne
    let two = d |> Seq.where (Seq.length >> (=) 5) |> Seq.except [five ; three] |> Seq.exactlyOne
    let six = d |> Seq.where (Seq.length >> (=) 6) |> Seq.except [nine] |> Seq.where (fun x -> five |> Seq.except x |> Seq.isEmpty) |> Seq.exactlyOne
    let zero = d |> Seq.except [one;two;three;four;five;six;seven;eight;nine] |> Seq.exactlyOne

    let mapper = 
        Map [ 
            (zero   |> Concat, "0"); 
            (one    |> Concat, "1"); 
            (two    |> Concat, "2"); 
            (three  |> Concat, "3"); 
            (four   |> Concat, "4"); 
            (five   |> Concat, "5"); 
            (six    |> Concat, "6"); 
            (seven  |> Concat, "7"); 
            (eight  |> Concat, "8"); 
            (nine   |> Concat, "9") ]

    digits 
    |> Array.skip 10
    |> Array.map (Seq.sort >> Concat >> fun x -> mapper.[x])
    |> Array.reduce (+)
    |> int


"inputs/day08.txt" 
|> System.IO.File.ReadAllLines 
|> Array.map (split [|" | "; " "|])
|> Array.map decipherSegments
|> Array.sum
|> printfn "%A" 
