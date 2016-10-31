// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
(* Auto-generated code below aims at helping you parse *)
(* the standard input according to the problem statement. *)
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
  let linesArray : string list [] = 
    [| for i in 0..H - 1 -> Console.In.ReadLine() |> chunkifyLine |]
  
  let asciiPartsList : string [] list = 
    [ for j in 0..linesArray.[0].Length - 1 -> Array.init H (fun i -> linesArray.[i].[j]) ]
  
  let chars = 
    [ for c in 'A'..'Z' -> c ]
    @ [ '?' ]
  
  asciiPartsList
  |> List.zip chars
  |> dict

let alphabet = readAlphabet()

let toAscii (ls : IDictionary<char, string []>) (txt : string) : string [] list = 
  let ascii = 
    txt
    |> Seq.map (fun c -> 
         match Char.ToUpperInvariant(c) with
         | C when C >= 'A' && C <= 'Z' -> ls.[C]
         | _ -> ls.['?'])
    |> Seq.toList
  ascii

let printAscii (listOfLetters : string [] list) : unit =
  for i in 0..H - 1 do
    listOfLetters
    |> List.fold (fun s asciiParts -> s + (asciiParts.[i])) ""
    |> printfn "%s"

T
|> toAscii alphabet
|> printAscii
(* Write an action using printfn *)
(* To debug: Console.Error.WriteLine("Debug message") *)

printfn "answer"

[<EntryPoint>]
let main argv = 
  printfn "%A" argv
  0 // return an integer exit code
