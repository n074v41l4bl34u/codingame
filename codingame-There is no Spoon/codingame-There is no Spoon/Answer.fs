module Answer

open System

let input = Console.In.ReadLine
let w,h = input() |> int,input() |> int
let a2 = Array.init h (fun _ -> input())

let rec printfNR i j =
  if i=w then (-1,-1)
  else if a2.[j].[i]='0' then (i,j)
  else printfNR (i+1) j

let rec printfND i j =
  if j=h then (-1,-1)
  else if a2.[j].[i]='0' then (i,j)
  else printfND i (j+1)

let printN x y c = 
  match x,y,c with
  | i,j,'0' ->
    let x2,y2 = printfNR (i+1) j
    let x3,y3 = printfND i (j+1)
    printfn "%d %d %d %d %d %d" i j x2 y2 x3 y3
  | _ -> ()

a2 |> Array.iteri (fun y xs -> xs |> Seq.iteri (fun x c-> printN x y c ))