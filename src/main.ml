open Cil
open Printf
open List
open Cfg
open Set
open Map
open Dlgraph
open Dlthread
open Lockset

module Sm = Map.Make(String)

let create_yices_file g paths = 
	let oc = open_out "main.y" in
	Vss.iter (fun b -> 
		let rec genlist i ub = if i<ub then (String.concat "" ["v";string_of_int i])::(genlist (i+1) ub) else [] in
		let varlist = genlist 0 (List.length(Vs.elements b)) in
		let rec v2str l = match l with x::xb -> (g#label x)::v2str xb | _ -> [] in
		let vtxstrlist = v2str (Vs.elements b)  in
		let tplist = List.combine varlist vtxstrlist in
		List.iter (fun a-> match a with (v,s) -> fprintf oc "(define %s::bool)\n" (v)) tplist; (** variable declaration **)
		fprintf oc "(assert+ (not (and %s)) 10)\n" (String.concat " " varlist);
		let sl = List.map (fun s-> sprintf "(not %s)" s) varlist in
			fprintf oc "(assert+ (not (and %s)) 10)\n" (String.concat " " sl);
		List.iter (fun a-> fprintf oc "(assert+ %s 8)\n" (fst a)) tplist;
		fprintf oc "(max-sat)";
		) paths;
	close_out oc;;


let persist gList = 
	let () = List.iter
	begin
		fun g-> 
			begin
			let g' = g#transitive_closure in
				g#iter_vertex 
				begin
					fun v0->
					g#iter_vertex	
						begin
						fun v1-> if not(g#mem_edge v0 v1)&&(g#mem_edge v1 v0)  then							
							let v0' = g'#find_string_vertex (g#label v0) in
							let v1' = g'#find_string_vertex (g#label v1) in
							
							if g'#mem_edge v0' v1' then
								let path = Vs.empty in
								let path = Vs.add v0 path in
								let res = Vss.empty in
								let res = g#search v0 v1 v0 3 path res in
									 create_yices_file g res ;
									 Vss.iter (fun b -> printf "%d\n" (List.length (Vs.elements b))) res							
						end
				end
			end
	end gList
	in ()

class mainVisitor = object (self)
	inherit nopCilVisitor
	
	val mutable threadFunm = Sm.empty
	
	method vglob (gv : global) : global list visitAction = 
		match gv with
		GVar(vinfo , _,_) -> 
			begin
				match vinfo.vtype with 
					| TNamed(typeinfo,_) -> 
						begin
							if typeinfo.tname="pthread_mutex_t" 
								then () 
							else (); 
							DoChildren;
						end
 					| _ ->  DoChildren;
			end
		| _ -> DoChildren;
		
	
	method vfunc (f : fundec) : fundec visitAction = 
		if f.svar.vname = "main" 
			then 
				begin
					let tss = Tss.empty in
					let tss = collect_thread_create f.sbody.bstmts tss in
					let graphList = marklockset_tss tss threadFunm in
					let ()= persist graphList in 
					
					(* let () = List.iter (fun g-> let g'= g#transitive_closure in 
												let () = g'#print_edges
												in g#print_edges) graphList in *)
												
					DoChildren;
				end
		else 
			begin
				threadFunm <- Sm.add f.svar.vname f threadFunm; 
				DoChildren;
			end
end

let f = Frontc.parse "deadlock.cil.c" ();;

visitCilFileSameGlobals (new mainVisitor) f
