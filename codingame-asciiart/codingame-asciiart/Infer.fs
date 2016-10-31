open System
open Microsoft.FSharp.Core
open System.Collections.Generic

let L = int (Console.In.ReadLine())
let H = int (Console.In.ReadLine())
let T = Console.In.ReadLine()

let chunkifyLine (line : string) : string list = 
  [ for i in 0..L..line.Length - 1 do
      let max = Math.Min(i + L, line.Length) - 1
      yield line.[i..max] ]

let readAlphabet() = 
  let linesArray = 
    [| for _ in 0..H - 1 -> Console.In.ReadLine() |> chunkifyLine |]
  
  let asciiPartsList = 
    [ for j in 0..linesArray.[0].Length - 1 -> Array.init H (fun i -> linesArray.[i].[j]) ]
  
  let chars = 
    [ for c in 'A'..'Z' -> c ]
    @ [ '?' ]
  
  asciiPartsList
  |> List.zip chars
  |> dict

let toAscii (alpha : IDictionary<char, string []>) (txt : string) = 
  let ascii = 
    txt
    |> Seq.map (fun c -> 
         match Char.ToUpperInvariant(c) with
         | C when C >= 'A' && C <= 'Z' -> alpha.[C]
         | _ -> alpha.['?'])
    |> Seq.toList
  ascii

let printAscii (listOfLetters : string [] list) = 
  for i in 0..H - 1 do
    listOfLetters
    |> List.fold (fun s asciiParts -> s + asciiParts.[i]) ""
    |> printfn "%s"

let alphabet = readAlphabet()

T
|> toAscii alphabet
|> printAscii
