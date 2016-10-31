module AnswerC
open System
open System.IO

//let error x=Console.Error.WriteLine(sprintf "%A" x)

let input()=Console.In.ReadLine()
let N,Q = int(input()), int(input())

let mimeTypeDict =
  seq{for _ in 0..N-1 do yield input().Split ' '}
  |> Seq.map (fun emt->emt.[0].ToLowerInvariant(),emt.[1])
  |> dict   

//error mimeTypeDict

seq{for _ in 0..Q-1 do yield input().ToLowerInvariant()}
|> Seq.map (Path.GetExtension >> (fun e-> //error e;
                                          match e with
                                          | "" -> "UNKNOWN"
                                          | _ -> if mimeTypeDict.ContainsKey e.[1..] then mimeTypeDict.[e.[1..]] else "UNKNOWN"))
//|> Seq.iter error
|> Seq.iter (printfn "%s")