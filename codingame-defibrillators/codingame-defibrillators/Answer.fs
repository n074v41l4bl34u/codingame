module Answer

open System

let input () = Console.ReadLine()
let toRad f = f * Math.PI/180.0
let toFloat (s:string) = s.Replace(',','.') |> float |> toRad
let lonU,latU,n = toFloat(input()), toFloat(input()), int(input())

let db =
  Array.init n (fun _ -> input().Split ';')
  |> Array.map (fun d -> d.[1],toFloat(d.[4]),toFloat(d.[5]))

let dist (lonD,latD) =
  let x = (lonD - lonU) * Math.Cos((latD + latU)/2.0)
  let y = (latD - latU)
  Math.Sqrt(x*x + y*y) * 6371.0

db |> Array.map (fun (n,lon,lat) -> n,dist (lon,lat)) |> Array.minBy snd |> fst |> printfn "%s"