// puzzle: https://adventofcode.com/2021/day/2
#time

let toCommand (c: string) = c.Split ' ' |> fun x -> (x.[0], int x.[1])
let commands = "inputs/day02.txt" |> System.IO.File.ReadAllLines |> Seq.map toCommand

let position commands =
    let folder (distance, depth) (direction, amount) = 
        match direction with
        | "up"   -> distance, depth - amount
        | "down" -> distance, depth + amount
        | _      -> distance + amount, depth
    ((0,0), commands) ||> Seq.fold folder |> fun (distance, depth) -> distance * depth

let position2 commands =
    let folder (distance, depth, aim) (direction, amount) =
        match direction with
        | "up"   -> distance, depth, aim - amount
        | "down" -> distance, depth, aim + amount
        | _      -> distance + amount, depth + (amount * aim), aim
    ((0,0,0), commands) ||> Seq.fold folder |> fun (distance, depth, _) -> distance * depth

// Answer 1
commands |> position |> printfn "%i"

// Answer 2
commands |> position2 |> printfn "%i"

