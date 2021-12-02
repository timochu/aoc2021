// puzzle: https://adventofcode.com/2021/day/2
#time

type Command =  { Direction: string; Amount: int }

let toCommand (c: string) =
    let parts = c.Split ' '
    { Direction = parts.[0]
      Amount = parts.[1] |> int }

let commands = "inputs/day02.txt" |> System.IO.File.ReadAllLines |> Seq.map toCommand

let position commands =
    ((0,0), commands) 
    ||> Seq.fold (fun (distance, depth) command -> 
        match command.Direction with
        | "up" ->  (distance, depth - command.Amount)
        | "down" ->  (distance, depth + command.Amount)
        | _ ->  (distance + command.Amount, depth))
    |> fun (distance, depth) -> distance * depth

let position2 commands =
    ((0,0,0), commands)
    ||> Seq.fold (fun (distance, depth, aim) command -> 
        match command.Direction with
        | "up" ->  (distance, depth, aim - command.Amount)
        | "down" ->  (distance, depth, aim + command.Amount)
        | _ ->  (distance + command.Amount, depth + (command.Amount * aim), aim))
    |> fun (distance, depth, _) -> distance * depth

// Answer 1
commands |> position |> printfn "%i"

// Answer 2
commands |> position2 |> printfn "%i"

