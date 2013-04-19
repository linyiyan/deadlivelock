open Cil
open Printf
open List
open Cfg
open Set
open Map
open Dlgraph
open Dlthread
open Lockset
open Yicesgen
open Scanf
module Sm = Map.Make(String)



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
end ;;


let fstr = Array.get Sys.argv 1 in
let commstr = sprintf "PATH=$PATH:/home/henry/cil-1.6.0/bin | export PATH | cilly --save-temps -D HAPPY_MOOD example/simple/%s.c -lpthread" fstr in
let fstr = sprintf "%s.cil.c" fstr in
let _ = Sys.command commstr in
let f = (Frontc.parse fstr) () in
visitCilFileSameGlobals (new mainVisitor) f
