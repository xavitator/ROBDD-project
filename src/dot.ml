open Graph

module MakeDot (Comparator : sig val is_lower : int -> int -> bool val is_unite : int -> bool end) =
struct 
  (* representation of a node -- must be hashable *)
  module Node = struct
    type t = int*string
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
      let edge_attributes (a,e,b) = if Comparator.is_lower (fst a) (fst b) then [`Style `Dotted] else [`Style `Solid]
      let default_edge_attributes _ = []
      let get_subgraph _ = None
      let vertex_attributes (i, name) = if Comparator.is_unite i = false then [`Label name] else [`Label (if i = 0 then "false" else "true"); `Shape `Box]
      let vertex_name (i,name) = string_of_int i
      let default_vertex_attributes _ = []
      let graph_attributes _ = []
    end)

  let g = ref G.empty

  let table = Hashtbl.create 4096
  let content = Hashtbl.create 4096

  let same_rank hshtbl =
    let aux a b tmp =
      (tmp ^ " " ^ (string_of_int b))
    in
    "  {rank=same" ^ (Hashtbl.fold aux hshtbl "") ^ "}\n"

  let all_rank () =
    Hashtbl.fold (fun a b c -> c ^ (same_rank b)) table ""

  let output_file fic = 
    let file = open_out_bin fic in
    Dot.output_graph file !g; 
    seek_out file (pos_out file - 1);
    output_string file (all_rank ());
    if(Hashtbl.mem content 1 && Hashtbl.mem content 0) then output_string file "  {rank=same 0 1}\n";
    output_string file " }";
    g := G.empty; Hashtbl.clear table; Hashtbl.clear content

  let add_node el =
    if Hashtbl.mem content (fst el) = false then begin
      let (i, name) = el in
      begin
        if Hashtbl.mem table name then Hashtbl.add (Hashtbl.find table name) i i
        else 
          begin
            let contenu = Hashtbl.create 4096 in 
            begin
              Hashtbl.add contenu i i;
              Hashtbl.replace table name contenu
            end
          end;
        let node = G.V.create el in
        g := G.add_vertex !g node;
        Hashtbl.replace content (fst el) node
      end
    end

  let add_liaison n1 n2 =
    let node1 = Hashtbl.find content (fst n1) in
    let node2 = Hashtbl.find content (fst n2) in
    g := G.add_edge !g node1 node2

end