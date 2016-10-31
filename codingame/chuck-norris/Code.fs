module Code

(* Auto-generated code below aims at helping you parse *)
(* the standard input according to the problem statement. *)
open System
open System.Text.RegularExpressions

//let (|ParseRegex|_|) regex str =
//   let m = Regex(regex).Match(str)
//   if m.Success
//   then Some (List.tail [ for x in m.Groups -> x.Value ])
//   else None
//
//let (|PaRe|_|) regex str =
//  let re = Regex(regex)
//  let mc = re.Matches(str);
//  Array.init mc.Count (fun i->mc.[i]) |> Some
//
////  matches|>Array.iter (fun m->printfn "%A" m)
////  printfn "end"
////  let mutable mIdx=0;
////  for m in mc do
////    for gIdx in 0 .. m.Groups.Count do
////      Console.WriteLine("[{0}][{1}] = {2}", mIdx, re.GetGroupNames().[gIdx], m.Groups.[gIdx].Value)
////    mIdx<-mIdx+1
////  None
//
////let (|Zero|_) input =
////  _
//
//let rec encode state input =
//  match input with
////  | Zero x -> encode input (matched_value_encoded + state)
////  | One x -> encode input (matched_value_encoded + state)
//  | PaRe "(0*[^1])|(1*[^0])" a
//          -> encode ("0" + state) input
//  | ParseRegex "(0*[^1])|(1*[^0])" a
//          -> encode ("0" + state) input
//  | ParseRegex "(0*[^1])" a
//          -> encode ("00" + state) input
//  | ParseRegex "(1*[^0])" a
//          -> encode ("0 000" + state) input
//  | _ -> state
let enc i c l =
  let str = (if i=0 then "" else " ") + (if c='0' then "00" else "0") + " " + String('0',l)
  str

let encode0 str =
  let mc = Regex("(0*[^1])|(1*[^0])").Matches(str)
  Array.init mc.Count (fun i->mc.[i])
  |> Array.map (fun m->m.Value.[0],m.Length)
  |> Array.mapi (fun i (c,l) -> (if i=0 then "" else " ") + (if c='0' then "00" else "0") + " " + String('0',l) )
  |> Array.reduce (+) 

let read (str:string) =
  str
  |> Seq.map (fun m -> Convert.ToString(int m, 2))
  |> Seq.reduce (+)
  |> encode0
  |> printfn "%s"

//Console.In.ReadLine() |> read

(* Write an action using printfn *)
(* To debug: Console.Error.WriteLine("Debug message") *)
