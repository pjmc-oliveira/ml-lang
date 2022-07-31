(** This module takes in a graph and returns the sorted strongly connected
    componenents of it
    source: https://www.programiz.com/dsa/strongly-connected-components *)
module Make (Ord : sig
  type t

  val compare : t -> t -> int
end) =
struct
  module OrdMap = Map.Make (Ord)
  module OrdSet = Set.Make (Ord)

  type t = {
    edges : Ord.t list OrdMap.t;
        (** A map from nodes to things they depend on *)
    visited : OrdSet.t;  (** A set of visited nodes *)
    stack : Ord.t list;  (** A FILO stakc of the current traversal order *)
  }
  (** The representation of the graph *)

  (** Creates a new graph *)
  let make edges = { edges; visited = OrdSet.empty; stack = [] }

  (** Get the first key to visit *)
  let first_key (g : t) =
    let keys =
      List.map (fun (k, _) -> k) (List.of_seq (OrdMap.to_seq g.edges))
    in
    let unvisted_keys =
      List.filter (fun k -> not (OrdSet.mem k g.visited)) keys
    in
    match unvisted_keys with
    | [] -> None
    | k :: _ -> Some k

  (** Get the list of unvisited keys from a specified node *)
  let unvisited_from (g : t) node =
    match OrdMap.find_opt node g.edges with
    | None -> []
    | Some nodes -> List.filter (fun n -> not (OrdSet.mem n g.visited)) nodes

  (** Visit a node *)
  let visit name g = { g with visited = OrdSet.add name g.visited }

  (** Push a node in the stack *)
  let push name g = { g with stack = name :: g.stack }

  (** Returns a reversed copy of a graph *)
  let reverse (g : t) =
    let edges =
      OrdMap.fold
        (fun node neigbours g ->
          let g' =
            List.fold_left
              (fun g n ->
                match OrdMap.find_opt n g with
                | None -> OrdMap.add n [ node ] g
                | Some ns -> OrdMap.add n (node :: ns) g)
              g neigbours
          in
          g')
        g.edges OrdMap.empty
    in
    make edges

  (** Explores unvisited nodes from this node *)
  let rec explore (curr : Ord.t) (g : t) =
    let unvisited = unvisited_from g curr in
    let g' =
      (* Explore each unvisited node *)
      List.fold_left (fun g next -> explore next (visit next g)) g unvisited
    in
    (* Push itself on the stack *)
    push curr g'

  (** Traverses and pushes all visited nodes into the stack *)
  let rec traverse (g : t) =
    match first_key g with
    | None -> g
    | Some start -> explore_all start g

  and explore_all (start : Ord.t) (g : t) =
    let g' = explore start (visit start g) in
    let n_visited = Seq.length (OrdSet.to_seq g'.visited) in
    let n_total = Seq.length (OrdMap.to_seq g'.edges) in
    if n_visited >= n_total then
      g'
    else
      traverse g'

  (** Get strongly connected components of a reversed graph *)
  let get_scc (g : t) =
    let children_of g node =
      match OrdMap.find_opt node g.edges with
      | None -> []
      | Some nodes -> nodes
    in
    let rec visit_children component to_visit g =
      match to_visit with
      | [] -> (component, g)
      | next :: to_visit' when OrdSet.mem next g.visited ->
          visit_children component to_visit' g
      | next :: to_visit' ->
          let component' = next :: component in
          visit_children component'
            (to_visit' @ children_of g next)
            (visit next g)
    in
    let rec loop acc g =
      match g.stack with
      | [] -> acc
      | node :: stack when OrdSet.mem node g.visited ->
          loop acc { g with stack }
      | node :: stack ->
          let children = children_of g node in
          let component, g' =
            visit_children [ node ] children (visit node { g with stack })
          in
          loop (component :: acc) g'
    in
    loop [] g

  (** Traverses the graph to find the strongly connected compoenents *)
  let run (g : t) =
    let g' = traverse g in
    let g'' = reverse g' in
    let g'' = { g'' with stack = g'.stack } in
    let scc = get_scc g'' in
    scc
end