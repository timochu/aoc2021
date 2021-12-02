// puzzle: https://adventofcode.com/2021/day/2
#time

type Direction = Up | Down | Forward
type Command = 
    { Direction: Direction
      Amount: int }

let toCommand (c: string) =
    let parts = c.Split ' '
    { Amount = parts.[1] |> int
      Direction = parts.[0] |> function 
        | "up" -> Up
        | "down" -> Down
        | _ -> Forward
       }

let commands = "inputs/day02.txt" |> System.IO.File.ReadAllLines |> Seq.map toCommand

let down = commands |> Seq.where (fun c -> c.Direction = Down) |> Seq.sumBy (fun c -> c.Amount)
let up = commands |> Seq.where (fun c -> c.Direction = Up) |> Seq.sumBy (fun c -> c.Amount)
let forward = commands |> Seq.where (fun c -> c.Direction = Forward) |> Seq.sumBy (fun c -> c.Amount)
let answer1 = (down-up)*forward
printfn "%i" answer1