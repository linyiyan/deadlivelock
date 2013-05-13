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
open Yicesgen
open Maxsat_file
open Scanf
module Sm = Map.Make(String)
module Ss = Set.Make(String)
module Sss = Set.Make(Ss)
	

let gen_cyclic_lock_dep gList = 
	let deplist = List.fold_left
	begin
		fun deplist g-> 
		begin
		let g' = g#transitive_closure in
		let vl = g#list_vertices in
		let res = List.fold_left 
			begin
				fun rs v0-> 
					let tres = List.fold_left
					begin
						fun trs v1 -> 
						let vlsts = enum_paths v0 v1 g g' in
						let strlsts = List.fold_left
							begin 
							fun strlsts vlst -> 
								let strlst = vertex_lst_to_str_lst g vlst in 								
									strlst :: strlsts;								
							end [] vlsts in
						strlsts @ trs 
					end [] vl 
					in tres @ rs;
			end [] vl
		in  res @ deplist
		end 
	end [] gList
	in  deplist	
	
(* is valid dependency *)
let isValidDep (dep : string list) : bool = 
	let rec risValidDep dep funccache holdlockcache= 
		match dep with
		| stmt::xdep -> 
			let (tf,locklst,_,_) = parse_stmt_str_info stmt in 
				if(Ss.mem tf funccache) || (List.exists (fun lk -> Ss.mem lk holdlockcache) locklst) then false 
				else 
					begin
						let funccache=Ss.add tf funccache in 
						let holdlockcache=List.fold_left (fun cache lk -> Ss.add lk cache) holdlockcache locklst in
						risValidDep xdep funccache holdlockcache
					end
		| _ -> true
	in risValidDep dep Ss.empty Ss.empty	


let gen_unique_cyclic_lock_dep gList = 
	let deplist = gen_cyclic_lock_dep gList in
	let depset = Sss.empty in 
	let reslist = List.fold_left 
		begin
			fun reslist dep -> 
				let dep_s = list_to_set dep in
				if Sss.mem dep_s depset then reslist
				else 
				begin
					Sss.add dep_s depset;
					dep::reslist
				end
		end [] deplist
	in 
	let reslist = List.fold_left
	begin
		fun rl l -> 
			l::(List.filter (fun l'-> not (isRotation l' l)) rl)
	end reslist reslist 
	in 
	let reslist = List.filter (fun dep -> isValidDep dep) reslist
	in reslist

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
					let tss = Tss.empty in
					let tss = collect_thread_create f.sbody.bstmts tss in
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
					let dep_list = gen_unique_cyclic_lock_dep graphList
					in let stmt2satvarname = gen_stmt2satvarname dep_list in 
					let () = List.iter
						begin
							fun dep -> let dlpair_lst = compute_deadlivelock_pairs (dep) (threadFunm) 
								in 
								let () = List.iter (fun dlpair -> printf "dl pair: %s %s" (fst dlpair) (snd dlpair)) dlpair_lst
								in printf "\n"
						end dep_list
					in 
					let diff_tuple_lst = recovery_diff dep_list (hlper#get_stmt2lockset) (* (hlper#get_stmt2sharedvar) (hlper#get_stmt2funcdomain) *) in
					let rank_m = rank (stmt2satvarname) (diff_tuple_lst) in 
					let rank_m_bindings = Sm.bindings rank_m in
					let () = List.iter (fun (k,v) -> printf "%s -> %d\n" k v) rank_m_bindings in
					let () = gen_maxsat_file stmt2satvarname dep_list rank_m in
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
