open Graph

module MakeDot (Comparator : sig val is_lower : int -> int -> bool end) =
struct 
  (* representation of a node -- must be hashable *)
  module Node = struct
    type t = int
    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let equal = (=)
  end

  (* representation of an edge -- must be comparable *)
  module Edge = struct
    type t = string
    let compare = Pervasives.compare
    let equal = (=)
    let default = ""
  end 

  (* a functional/persistent graph *)
  module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Node)(Edge)

  (* module for creating dot-files *)
  module Dot = Graph.Graphviz.Dot(struct
      include G (* use the graph module from above *)
      let edge_attributes (a,e,b) = if Comparator.is_lower a b then [`Style `Dotted] else [`Style `Solid]
      let default_edge_attributes _ = [`Arrowhead `None]
      let get_subgraph _ = None
      let vertex_attributes _ = []
      let vertex_name = string_of_int
      let default_vertex_attributes _ = []
      let graph_attributes _ = []
    end)

  let g = ref G.empty

  let output_file fic = 
    let file = open_out_bin fic in
    Dot.output_graph file !g; g := G.empty

  let add_node el = 
    let node = G.V.create el in
    g := G.add_vertex !g node; node

  let add_liaison n1 n2 =
    let node1 = add_node n1 in
    let node2 = add_node n2 in
    g := G.add_edge !g node1 node2

end