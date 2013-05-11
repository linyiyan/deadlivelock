open Dlutil
open Map
open Set
open Printf
module Sm = Map.Make(String)
module Ss = Set.Make(String)

(* count #locks need to be release in cyclic dependency *)
let count_release_locks (dep:string list) (stmt2lockset : Ss.t Sm.t) (res : int Sm.t) : (int Sm.t) = 
	let dep = (last_elem dep) :: dep in
	let rec rcount_release_locks (dep:string list) (stmt2lockset : Ss.t Sm.t) (res : int Sm.t) : (int Sm.t) = 
	match dep with
	| d1::d2::xdep -> 
		let (threadFunc,_,_,rel_lock) = parse_stmt_str_info d1 in 
		let roll_back_to_str = find_lock_str threadFunc rel_lock stmt2lockset in 
		if roll_back_to_str="" then 
			let res = Sm.add d2 1 res in 
			rcount_release_locks (d2::xdep) stmt2lockset res 
		else
			let (_,hl,_,_) = parse_stmt_str_info d2 in
			let (_,rhl,_,_) = parse_stmt_str_info roll_back_to_str in 
			(*let delta = (List.length hl) - (List.length rhl) in *)
			let lkset1 = Sm.find d2 stmt2lockset in
			let lkset2 = Sm.find roll_back_to_str stmt2lockset in
			let delta = Ss.cardinal (Ss.diff lkset1 lkset2) in
			let res = Sm.add d2 delta res in 
			rcount_release_locks (d2::xdep) stmt2lockset res 
	| _ -> res
	in rcount_release_locks dep stmt2lockset res

(* compute recovery difficulty, return a tuple list representing relations between any two lock statements*)
let recovery_diff (dep_list : string list list)
				(stmt2lockset : Ss.t Sm.t) 
			(*	(stmt2sharedvar : Ss.t Sm.t)
				(stmt2funcdomain : Ss.t Sm.t) *)
				: (string*string*string) list= 
	let stmt2num_rel_lock = 
		List.fold_left
		begin
			fun rm dep ->count_release_locks (dep) (stmt2lockset) rm
		end Sm.empty dep_list
	in
	(* let stmt2num_reset_sharedvar_updates = Sm.empty 
	in 
	let stmt2funcdomain = Sm.empty
	in *)
	let stmtlst = fst (List.split (Sm.bindings stmt2lockset)) 
	in let stmttuplelst = unique_combinations2 stmtlst
	in let stmttuplelst = List.filter
							begin
								fun tple ->
									let (_,_,opr1,_), (_,_,opr2,_) = parse_stmt_str_info(fst tple),parse_stmt_str_info(snd tple) in
									if opr1="pthread_mutex_lock" && opr2="pthread_mutex_lock" then true
									else false
							end stmttuplelst	
	in List.fold_left
		begin
			fun rlst tple -> 
				if not (Sm.mem (fst tple) stmt2num_rel_lock) || not (Sm.mem (snd tple) stmt2num_rel_lock) then rlst
				else
				let numl1,numl2 = (Sm.find (fst tple) stmt2num_rel_lock), (Sm.find (snd tple) stmt2num_rel_lock) (* compare #locks need to be released*)
				in if numl1 < numl2 then ((fst tple),(snd tple),"<") :: rlst
					else if numl1 > numl2 then ((fst tple),(snd tple),">") :: rlst
					else ((fst tple),(snd tple),"=") :: rlst
		end [] stmttuplelst
	


	
	
	