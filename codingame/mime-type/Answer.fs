module Answer
open System
open System.IO
open Microsoft.FSharp.Collections

let input() = Console.In.ReadLine()
let N, Q = int (input()), int (input())

let mimeTypeDict = 
  Seq.init N (fun _ -> input().Split ' ')
  |> Seq.map (fun emt -> emt.[0].ToLowerInvariant(), emt.[1])
  |> dict

Seq.init Q (fun _ -> input().ToLowerInvariant())
|> Seq.map (Path.GetExtension >> (fun e -> 
            match e with
            | "" -> "UNKNOWN"
            | _ when mimeTypeDict.ContainsKey e.[1..] -> mimeTypeDict.[e.[1..]]
            | _ -> "UNKNOWN"))
|> Seq.iter (printfn "%s")
