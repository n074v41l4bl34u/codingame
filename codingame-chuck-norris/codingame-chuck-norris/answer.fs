module Answer

open System
open System.Text.RegularExpressions

let encode str =
  let mc = Regex("(0*[^1])|(1*[^0])").Matches(str)
  Array.init mc.Count (fun i->mc.[i])
  |> Array.map (fun m->m.Value.[0],m.Length)
  |> Array.mapi (fun i (c,l) -> (if i=0 then "" else " ") + (if c='0' then "00" else "0") + " " + String('0',l) )
  |> Array.reduce (+) 

Console.In.ReadLine()
|> Seq.map (fun m -> Convert.ToString(int m, 2))
|> Seq.map (fun s -> String('0',7-s.Length)+s)
|> Seq.reduce (+)
|> encode
|> printfn "%s"