module Answer

open System

type Link = int*int
type Node = {Id:int;Links:Set<Link>}
type Graph = {Ids:int list;Links:Set<Link>} with
  static member inline (+) (n : Node, g : Graph) : Graph = {Ids=n.Id::g.Ids; Links=n.Links+g.Links}

let error x = x |> sprintf "%A" |> System.Console.Error.WriteLine
let sw = System.Diagnostics.Stopwatch()

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
let graph = {Ids=List.init N (fun i->i); Links=links}

//error "Links";links |> Set.iter error
//error "Exits";exits |> List.iter error
//error "Graph:";error graph

let (|DecomposeGraph|) nid g : Node*Graph option =
  match g with
  | {Ids=_;Links=lss} -> //will cause error if Ids does not contain nid
    {Id=nid; Links=lss |> Set.filter (fun (ls0,ls1)->ls0=nid || ls1=nid)}, match {Ids=g.Ids |> List.filter (fun ns0->ns0<>nid); Links=lss |> Set.filter (fun (ls0,ls1)->ls0<>nid && ls1<>nid)} with 
                                                                                  |{Ids=[];Links=_} -> None 
                                                                                  | g0->Some g0
  //| _ -> failwith "non decomposable graph"

let decomposeGraphFrom2 (g:Graph) (nid:int) : Link list*Graph option= 
  //error ("decomposeGraphFrom2",nid,g)
  let n0,g0o = (|DecomposeGraph|) nid g
  //error ("retval",n0,g0o)
  n0.Links|>Set.toList,g0o

let isAgentLink si (l0,l1) = l0=si || l1=si

let ensureLink (orgLink:Option<Link>) link = if orgLink.IsSome then orgLink.Value else link
let ensureRootLink (orgLink:Option<Link>) link = if orgLink.IsSome then orgLink else Some link

let extractRootFromLink0 prevRoot (l0,l1) =
  if l0=prevRoot then l1 else l0

//let extractRootFromLink prevRoot lo (l0,l1) =
//  if l0=prevRoot then l1 else l0,lo
//
//let extractRootsFromLinks prevRoot lo links =
//  links |>Seq.map (extractRootFromLink prevRoot lo)

let checkRoots graph si roots =
  let isAgentLinkSi = isAgentLink si
  let rec linkCandidates roots0 =
    let decompositions =
      roots0 
      |> Seq.map (fun (graph0,i,lo)->decomposeGraphFrom2 graph0 i,i,lo)
      |> Seq.collect (fun ((matchCandidates,go),i,lo) -> matchCandidates|>Seq.map(fun mc->mc,go,i,ensureRootLink lo mc))
      |> Seq.cache
    let found =
      decompositions
      |>Seq.tryFind (fun (mc,_,_,_)->isAgentLinkSi mc)
//      |> Seq.map (fun ((matchCandidates,graphOpt),i,lo) ->
//        match matchCandidates|>Seq.tryFind isAgentLinkSi,graphOpt with
//        | Some link,_ -> ensureLink lo link //<initializedLink or link>
//        | None,Some g-> matchCandidates|> (extractRootsFromLinks i lo)|> (linkCandidates g)|>Seq.head
//        | _ -> failwith "Empty graph and no link found")
    if found.IsSome then 
      let link,_,_,lo=found.Value
      ensureLink lo link
    else 
      decompositions
      |> Seq.filter (fun (_,go,_,_)->go.IsSome)
      |> Seq.map (fun (mc,g,i,lo)-> (g.Value,(extractRootFromLink0 i mc),lo))
      |> linkCandidates

  roots
  |> Seq.map(fun i->graph,i,None)
  |> linkCandidates

let nearestCutFrom2 (g:Graph) (si:int):Link*Graph=
  let link = checkRoots g si exits
  link,{g with Links=g.Links-Set.singleton link }

let findNearestCut2 g si =
  let (l0,l1),g2 = nearestCutFrom2 g si
  l0,l1,g2

let rec gameLoop g =
  let l0,l1,g2 = input() |> int |> findNearestCut2 g
  printfn "%d %d" l0 l1
  gameLoop g2

sw.Start()
gameLoop graph
sw.Stop()
error sw.ElapsedMilliseconds