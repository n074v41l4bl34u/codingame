// constraints
//  2 ≤ surfaceN < 30
//  0 ≤ X < 7000
//  0 ≤ Y < 3000
//  -500 < hSpeed, vSpeed < 500
//  0 ≤ fuel ≤ 2000
//  -90 ≤ rotate ≤ 90
//  0 ≤ power ≤ 4
// landing
//  For a landing to be successful, the ship must:
//  land on flat ground
//  land in a vertical position (tilt angle = 0°)
//  vertical speed must be limited ( ≤ 40m/s in absolute value)
//  horizontal speed must be limited ( ≤ 20m/s in absolute value)
// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
printfn "0 1"
[<Measure>]
type m

[<Measure>]
type s

[<Measure>]
type speed = m / s
let toFloat (v:int<speed>)= (float v)*1.0<speed>

[<Measure>]
type acc = m / (s^2)

[<Measure>]
type l

[<Measure>]
type deg

type Input = 
  { X : int<m>
    Y : int<m>
    HSpeed : int<speed>
    VSpeed : int<speed>
    Fuel : int<l>
    Rotation : int<deg>
    Power : int<acc> }

let getInput() = 
  let token = (Console.In.ReadLine()).Split ' '
  { X = int token.[0] * 1<m>
    Y = int token.[1] * 1<m>
    HSpeed = int token.[2] *1<speed>
    VSpeed = int token.[3] *1<speed>
    Fuel = int token.[4] *1<l>
    Rotation = int token.[5] *1<deg>
    Power = int token.[6] *1<acc> }

type Output = 
  { Rotation : int
    Power : int }
  member x.Str = sprintf "%d %d" x.Rotation x.Power

let outputPower p =
    let p0 =
      match int p with
      | -1 -> 0
      | 5 -> 4
      | _ -> p
    {Rotation=0; Power=p0}.Str |> printfn "%s"

let gravityMars = -3.711<acc>
let surfaceN = int (Console.In.ReadLine())
  
(* the number of points used to draw the surface of Mars. *)
let ground = 
  [ for i in 0..surfaceN - 1 -> 
      (* landX: X coordinate of a surface point. (0 to 6999) *)
      (* landY: Y coordinate of a surface point. By linking all the points together in a sequential fashion, you form the surface of Mars. *)
      let token = (Console.In.ReadLine()).Split [| ' ' |]
      int (token.[0]), int (token.[1]) ]
  
(* game loop *)
//ground  |> sprintf "%A" |> Console.Error.WriteLine
//getInput() |> sprintf "%A" |> Console.Error.WriteLine
//printfn "0 3"
//getInput() |> sprintf "%A" |> Console.Error.WriteLine
//printfn "0 3"
//getInput() |> sprintf "%A" |> Console.Error.WriteLine
//printfn "0 3"
//getInput() |> sprintf "%A" |> Console.Error.WriteLine
//printfn "0 3"
//getInput() |> sprintf "%A" |> Console.Error.WriteLine
//printfn "0 3"
//getInput() |> sprintf "%A" |> Console.Error.WriteLine

//let rec gl () =
//  let input = getInput()
//  match input with
//  | i when i.VSpeed < 40<speed> -> printfn "0 3"
//  | _ -> printfn "0 2"
//  gl ()

//gl()

let rec gameLoop state = 
  let input = getInput()
  match input with
  | i when i.VSpeed < -40<speed> -> int (input.Power+1<acc>) |> outputPower
  | i when i.VSpeed < 0<speed> -> int (input.Power+1<acc>) |> outputPower
  | i when i.VSpeed > 40<speed> ->  int (input.Power-1<acc>) |> outputPower
  | i when i.Y > 0<m> ->  int (input.Power+1<acc>) |> outputPower
  | _ ->  int (input.Power+1<acc>) |> outputPower
  gameLoop state

gameLoop []

[<EntryPoint>]
let main argv = 
  printfn "%A" argv
  (* Auto-generated code below aims at helping you parse *)
(* the standard input according to the problem statement. *)

  while true do
    (* hSpeed: the horizontal speed (in m/s), can be negative. *)
    (* vSpeed: the vertical speed (in m/s), can be negative. *)
    (* fuel: the quantity of remaining fuel in liters. *)
    (* rotate: the rotation angle in degrees (-90 to 90). *)
    (* power: the thrust power (0 to 4). *)
    let token1 = (Console.In.ReadLine()).Split [| ' ' |]
    let X = int (token1.[0])
    let Y = int (token1.[1])
    let hSpeed = int (token1.[2])
    let vSpeed = int (token1.[3])
    let fuel = int (token1.[4])
    let rotate = int (token1.[5])
    let power = int (token1.[6])
    (* Write an action using printfn *)
    (* To debug: Console.Error.WriteLine("Debug message") *)
    (* 2 integers: rotate power. rotate is the desired rotation angle (should be 0 for level 1), power is the desired thrust power (0 to 4). *)
    printfn "0 3"
    ()
  0 // return an integer exit code
