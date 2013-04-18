open Graph
open Printf

module Sdg = Imperative.Digraph.Abstract(String)
module Vs  = Set.Make(Sdg.V)
module Vss = Set.Make(Vs)
module Sm = Map.Make(String)
module Ss = Set.Make(String)

class lockGraph = object (self)
	
	val mutable vertex_str_cache = Sm.empty
	
	val mutable g = Sdg.create()
	
	method create_str_vertex str = 
		if Sm.mem str vertex_str_cache then Sm.find str vertex_str_cache
		else let v=Sdg.V.create str in
			(*  printf "%s\n" str; *)
			 vertex_str_cache <- Sm.add str v vertex_str_cache ;
			 v
	
	method add_str_edge str0 str1 = 
		let v0 = self#create_str_vertex str0 in
		let v1 = self#create_str_vertex str1 in
			Sdg.add_edge g v0 v1;
			self
			
	method vertex_size = Sdg.nb_vertex g
end
