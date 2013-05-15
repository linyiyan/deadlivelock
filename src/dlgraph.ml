open Graph
open Printf
module Sdg = Imperative.Digraph.Abstract(String)
module Vs  = Set.Make(Sdg.V)
module Vss = Set.Make(Vs)
module Sm = Map.Make(String)
module Ss = Set.Make(String)
module Op = Oper.I(Sdg)
exception Found of Sdg.V.t

class lockGraph = object (self)
	
	val mutable vertex_str_cache = Sm.empty
	
	val mutable g = Sdg.create()
	
	method set_g ig = g<-ig
	
	method private create_str_vertex str = 
		if Sm.mem str vertex_str_cache then Sm.find str vertex_str_cache
		else let v=Sdg.V.create str in
			(*  printf "%s\n" str; *)
			 vertex_str_cache <- Sm.add str v vertex_str_cache ;
			 v
	
	method add_str_edge str0 str1 = 
		let v0 = self#create_str_vertex str0 in
		let v1 = self#create_str_vertex str1 in
			(* printf "%s ----> %s\n" str0 str1; *)
			Sdg.add_edge g v0 v1;
			self
			
	
	method transitive_closure =
		let ng = new lockGraph in 
		let () = ng#set_g (Op.transitive_closure ~reflexive:false g) in
		ng	

	method find_string_vertex i =
    try
      Sdg.iter_vertex (fun v -> if Sdg.V.label v = i then raise (Found v)) g;
      raise Not_found
    with Found v -> v
	
	(* method search (src : Sdg.V.t) (tgt : Sdg.V.t) (cur : Sdg.V.t) (k : int) (path : Vs.t) (res : Vss.t): Vss.t = 
		if(k>0 && tgt=cur) then Vss.add path res
		else if k<=0 then Vss.empty
		else 
			begin
				let succlst = Sdg.succ g cur in 
				List.fold_left 
					begin
						fun res sc -> 
							let path = Vs.add sc path in
							self#search src tgt sc (k-1) path res
					end res succlst
			end
		*)
		
	method search (src : Sdg.V.t) (tgt : Sdg.V.t) (cur : Sdg.V.t) (k : int) (path : Sdg.V.t list) (res : (Sdg.V.t list) list) : (Sdg.V.t list) list= 
		if(k>0 && tgt=cur) then ((*printf "path: "; List.iter (fun v -> printf "%s--" (Sdg.V.label v)) path ; printf "\n" ; *) path::res)
		else if k<=0 then []
		else
			begin
				let succlst = Sdg.succ g cur in 
				List.fold_left 
					begin
						fun res sc -> 
							let path = sc::path in (* printf "%s successor %s\n" (Sdg.V.label src)(Sdg.V.label sc); *)
							(self#search src tgt sc (k-1) path res)@res
					end res succlst				
			end
		
	method print_edges = 
		self#iter_vertex 
			begin
				fun v0 -> 
				self#iter_vertex
					begin
						fun v1 -> if self#mem_edge v0 v1 then printf "%s ----> %s\n" (self#label v0) (self#label v1)
					end
			end
			
	
	method label v = Sdg.V.label v
			
	method list_vertices_str = Sdg.fold_vertex (fun v vl -> (Sdg.V.label v)::vl) g []
	
	method list_vertices = Sdg.fold_vertex (fun v vl -> v::vl) g []
	
	method iter_vertex f = Sdg.iter_vertex (f) g
	
	method vertex_size = Sdg.nb_vertex g
	
	method edge_size = Sdg.nb_edges g
	
	method mem_edge v0 v1 = Sdg.mem_edge g v0 v1
	
	method print_all_edges = Sdg.iter_edges (fun v0 v1 -> printf "%s -> %s\n" (Sdg.V.label v0) (Sdg.V.label v1)) g
	
end

let enum_paths v0 v1 g g' = 
	if (* not(g#mem_edge v0 v1)&&*)(g#mem_edge v1 v0) then
		let v0' = g'#find_string_vertex (g#label v0) in
		let v1' = g'#find_string_vertex (g#label v1) in (*printf "%s %s\n" (g'#label v0')(g'#label v1'); g#print_all_edges;*)
		if g'#mem_edge v0' v1' then 
			let path = [v0] in (* printf "%s %s\n" (g#label v0)(g#label v1); *)
			let res = g#search v0 v1 v0 10 path [] in (*printf"res %d " (List.length res);*)res
		else []
	else []

