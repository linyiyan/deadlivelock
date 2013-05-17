open Cil
open Printf
open List
open Cfg
open Set
open Map
open Dlutil
open Deadlivelock
open Heuristic
open Dlgraph
open Dlthread
open Lockset
open Rank
open Maxsat_file
open Scanf
open Dependency
module Sm = Map.Make(String)
module Ss = Set.Make(String)
module Sss = Set.Make(Ss)
	

class mainVisitor file= object (self)
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
					let encode_beg = Unix.gettimeofday () in
					let tss = Tss.empty in
					let tss = collect_thread_create f.sbody.bstmts tss in (* print_tss tss; *)
					(*let graphList = marklockset_tss tss threadFunm in
					let ()= persist graphList in 	*)
					let hlper = new locksetHelper in
					let graphList = 
						Tss.fold 
							begin
								fun ts graphList -> 
									begin
										hlper#reset_acq_cache;
										hlper#reset_hold_cache;
										hlper#reset_lockset;
										(hlper#marklockset_ts ts threadFunm) :: graphList
									end
							end tss []
					in
					let dep_list = gen_unique_cyclic_lock_dep graphList in print_str_lsts dep_list;
					let stmt2satvarname = gen_stmt2satvarname dep_list in 
					let dlpair_lst = 
					List.fold_left
					begin
						fun rs dep ->
							let pair_lst = compute_deadlivelock_pairs (dep) (threadFunm) in rs@pair_lst
					end [] dep_list
					(* in List.iter (fun (s1,s2) -> printf "dlpair : %s || %s\n" (Sm.find s1 stmt2satvarname) (Sm.find s2 stmt2satvarname)) dlpair_lst;
					let () = List.iter
						begin
							fun dep -> let dlpair_lst = compute_deadlivelock_pairs (dep) (threadFunm) 
								in 
								let () = List.iter (fun dlpair -> printf "dl pair: %s %s\n" (fst dlpair) (snd dlpair)) dlpair_lst
								in printf "\n"
						end dep_list *)
					in 
					let diff_tuple_lst = recovery_diff dep_list (hlper#get_stmt2lockset) (* (hlper#get_stmt2sharedvar) (hlper#get_stmt2funcdomain) *) in
					let rank_m = rank (stmt2satvarname) (diff_tuple_lst) in 
					let rank_m_bindings = Sm.bindings rank_m in
					let encode_end = Unix.gettimeofday () in printf "encode time %fs\n" (encode_end -. encode_beg);
					let () = List.iter (fun (k,v) -> printf "%s -> %d\n" k v) rank_m_bindings in
					let () = gen_maxsat_file stmt2satvarname dep_list dlpair_lst rank_m in 
					DoChildren;
				end
		else 
			begin
				threadFunm <- Sm.add f.svar.vname f threadFunm; 
				DoChildren;
			end
end ;;

(*
let commstr = sprintf "cilly --save-temps -D HAPPY_MOOD example/simple/%s.c -lpthread" fstr in
let _ = Sys.command commstr in *)
let fstr = Array.get Sys.argv 1 in
let fstr = sprintf "%s.cil.c" fstr in
let f = (Frontc.parse fstr) () in
visitCilFileSameGlobals (new mainVisitor f) f
