module Answer

open System

type Link = int*int
type Node = {Id:int; Links:Set<Link>}

let isLinkPart nodeId (l0,l1) = l0=nodeId || l1=nodeId
type Graph = {Ids:int list;Links:Set<Link>} with
  static member inline (-) (g:Graph, nodeId:int) : Node*Option<Graph> = 
      {Id=nodeId; Links=g.Links |> Set.filter (isLinkPart nodeId)}, 
      match {Ids=g.Ids |> List.filter ((<>)nodeId); 
            Links=g.Links |> Set.filter (fun (l0,l1)->l0<>nodeId && l1<>nodeId)} with 
      |{Ids=[];Links=_} -> None 
      | newGraph -> Some newGraph

let input = Console.In.ReadLine
let input2I () = 
  let token = input().Split ' '
  int(token.[0]),int(token.[1])
let token = input().Split ' '
let N,L,E = int(token.[0]),int(token.[1]),int(token.[2])

let links = Array.init L (fun _ -> input2I())
let exits = Array.init E (fun _ -> int(input()))
let graph = {Ids=List.init N (fun i->i); Links=links |> Set.ofArray}

let getRootLink (rootLink:Option<Link>) link = if rootLink.IsSome then rootLink.Value else link
let ensureRootLink (rootLink:Option<Link>) link = if rootLink.IsSome then rootLink else Some link
let nextNodeFromLink prevRoot (l0,l1) = if l0=prevRoot then l1 else l0

let exploreLinks cutCandidates =
  cutCandidates 
  |> Seq.map (fun ((g:Graph),nodeId,rl)-> (g - nodeId),nodeId,rl)
  |> Seq.collect (fun ((node,graphPart),nodeId,rl) -> node.Links |> Seq.map (fun link-> link,graphPart,nodeId,ensureRootLink rl link))
  |> Seq.cache // store enumeration for performance reasons


let findNearestCut g si =
  let isAgentAtSi = isLinkPart si
  let rec findCut cutCandidates =
    let exploredLinks = exploreLinks cutCandidates
    let foundCut = exploredLinks |> Seq.tryFind (fun (mc,_,_,_) -> isAgentAtSi mc)

    match foundCut with
    |Some (link,_,_,rl) -> getRootLink rl link
    |None ->
      exploredLinks
      |> Seq.filter (fun (_,graphPart,_,_)-> graphPart.IsSome)
      |> Seq.map (fun (link,graphPart,nodeId,rl)-> graphPart.Value,nextNodeFromLink nodeId link,rl)
      |> findCut

  let cut=
    exits
    |> Seq.map(fun ei->g,ei,None)
    |> findCut

  cut,{g with Links=g.Links-Set.singleton cut }

let rec gameLoop g =
  let (l0,l1),g2 = input() |> int |> findNearestCut g
  printfn "%d %d" l0 l1
  gameLoop g2

gameLoop graph
