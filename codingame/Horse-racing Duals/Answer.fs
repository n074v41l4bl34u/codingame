module Answer

open System

let input() = Console.In.ReadLine() |> int
let N = input()
let diff (x:int,y:int) = Math.Abs(x-y)
Array.init N (fun _ -> input()) 
|> Array.sort 
|> Seq.pairwise 
|> Seq.minBy diff 
|> diff 
|> printf "%d"