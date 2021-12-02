// puzzle: https://adventofcode.com/2021/day/2
#time

let toCommand (line : string) = line.Split ' ' |> fun command -> command.[0], int command.[1]
let commands = "inputs/day02.txt" |> System.IO.File.ReadAllLines |> Seq.map toCommand

// Answer 1
commands
|> Seq.fold (fun (distance, depth) (direction, amount) ->
    match direction with
    | "up"   -> distance, depth - amount
    | "down" -> distance, depth + amount
    | _      -> distance + amount, depth) (0,0)
|> fun (distance, depth) -> distance * depth
|> printfn "%i"

// Answer 2
commands 
|> Seq.fold (fun (distance, depth, aim) (direction, amount) ->
    match direction with
    | "up"   -> distance, depth, aim - amount
    | "down" -> distance, depth, aim + amount
    | _      -> distance + amount, depth + (amount * aim), aim) (0,0,0)
|> fun (distance, depth, _) -> distance * depth 
|> printfn "%i"