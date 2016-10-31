module Answer

open System

type Link = int*int
type Node = {Id:int;Links:Set<Link>}
type Graph = {Ids:int list;Links:Set<Link>} with
  static member inline (+) (n : Node, g : Graph) : Graph = {Ids=n.Id::g.Ids; Links=n.Links+g.Links}
  //override x.ToString () = sprintf "%A; %A" x.Ids x.Links

let error x = x |> sprintf "%A" |> System.Console.Error.WriteLine
let input = Console.In.ReadLine
let input2I () = 
  let token = (input()).Split ' '
  int(token.[0]),int(token.[1])
(* N: the total number of nodes in the level, including the gateways *)
(* L: the number of links *)
(* E: the number of exit gateways *)
let token = (input()).Split ' '
let N,L,E = int(token.[0]),int(token.[1]),int(token.[2])

let links = Array.init L (fun _ -> input2I())|> Set.ofArray
let exits = List.init E (fun _ -> int(input()))
let graph = {Ids=List.init N (fun i->i);Links=links}

error "Links";links |> Set.iter error
error "Exits";exits |> List.iter error
error "Graph:";error graph

let graphExcept n g : Graph =
  {Ids=g.Ids |> List.filter (fun ns0->ns0<>n.Id); Links=g.Links - n.Links}

let (|GNHead|_|) n0 (g:Graph) : Node option =
  match g with
  | {Ids=[];Links=_} -> None
  | {Ids=ns;Links=lss} when ns|>List.exists (fun n->n=n0) -> Some {Id=n0; Links=lss |> Set.filter (fun (ls0,ls1)->ls0=n0 || ls1=n0)}
  | _ -> None

let (|DecomposeGraph|) nid g : Node*Graph option =
  match g with
  | {Ids=_;Links=lss} -> 
    {Id=nid; Links=lss |> Set.filter (fun (ls0,ls1)->ls0=nid || ls1=nid)}, match {Ids=g.Ids |> List.filter (fun ns0->ns0<>nid); Links=lss |> Set.filter (fun (ls0,ls1)->ls0=nid || ls1=nid)} with 
                                                                                  |{Ids=[];Links=_} -> None 
                                                                                  | g0->Some g0
  //| _ -> failwith "non decomposable graph"

//let (|GHead|_|) (g:Graph) : Node option =
//  match g with
//  | {Ids=[];Links=_} -> None
//  | {Ids=n::_;Links=lss} -> Some {Id=n; Links=lss |> Set.filter (fun (ls0,ls1)->ls0=n || ls1=n)}
  

// graph decomposition
// g ni -> n0,g0
// g ni -> n0,_

let decomposeGraphFrom2 (g:Graph) (nid:int) = 
  error ("decomposeGraphFrom2",nid,g)
  match (|DecomposeGraph|) nid g with
  | n0, Some g0 -> 1
  | n0, None -> 0


let decomposeGraphFrom (n0:int) (g:Graph) : Node option*Graph =
  error ("decomposeGraphFrom",n0,g)
  match g with  
  | GNHead n0 n -> Some n,graphExcept n g
//  | GHead n -> Some n,graphExcept n g
  | _ -> sprintf "%A+%A" n0 g |> error;  failwith "empty graph"


//let decomposeGraph (g:Graph) =
//  match g.Ids with
//  | [] -> None,g
//  | n0::_-> decomposeGraphFrom n0 g
   
//let decomposeGraph (g:Graph) : Node*Graph =
//  match g with
//  | GHead n -> n,graphExcept g n
//  | _ -> failwith "empty graph"

let distance si ei =
  let rec measureDistance (dist0:int) (dst:int) (node:Node) (gr:Graph) : int = // nodeLinks dist0 dst src =
    error ("measureDistance",dist0,dst,node,gr)
    if dst=node.Id then dist0
    else
      let ids = node.Links |> Set.map (fun (l0,l1)-> if l0=node.Id then l1 else l0)
      let decs = 
        [for ni in  ids ->
          let n0,g0 = decomposeGraphFrom ni gr
          error ("nth",n0,g0)
          match n0 with
          | None -> Int32.MaxValue
          | Some n00 -> measureDistance (dist0+1) dst n00 g0]
      decs |> List.min
  error ("init",ei,graph)
  let n,g = decomposeGraphFrom ei graph
  error ("1st",n,g)
  measureDistance 0 si n.Value g

let findNearestCut si =
  let ei =    exits    |> List.minBy (distance si)
  links |> Set.toArray |> Array.find (fun (l0,l1) -> l0=ei || l1=ei)

let nearestCutFrom (g:Graph) (exits:int list) (si:int) : int*Graph=
  let a = exits |> List.map (decomposeGraphFrom2 g)
  0,g

let findNearestCut2 g si =
  let ei,g = nearestCutFrom g exits si
  0,1,g

let rec gameLoop g =
  let l0,l1,g = input() |> int |> findNearestCut2 g
  printfn "%d %d" l0 l1
  gameLoop g

gameLoop graph