open Map
open Printf
open Scanf
open Dlutil
module Sm = Map.Make(String)

(* given a sat variable, check it represents a lock (not trylock) statement *)
let is_lock_var (v:string) (stmt2satvarname : string Sm.t) : bool = 
	let bdings = Sm.bindings stmt2satvarname in 
	let (stmt,_) = List.find (fun (_,v')-> v=v') bdings in 
	let (_,_,opr,_) = parse_stmt_str_info stmt in opr="pthread_mutex_lock"
	
(* get the subscript from the sat variable *)
let getsubscript_from_v (v:string) : int = 
	let f = fun v n -> (v,n) in
	let (_,n) = sscanf v "%c%d" f in n

let gen_maxsat_file (stmt2satvarname : string Sm.t) (deplist : string list list) (dlpair_list : (string*string) list) (rank_m : int Sm.t) =
	let var_num = Sm.cardinal stmt2satvarname in
	let max_rank = var_num + 1 in
	let res_str =
	List.fold_left
	begin
		fun dep_str dep->
			let temp1 = 
			List.fold_left
			begin
				fun stmt_str stmt-> 
					let var_name = Sm.find stmt stmt2satvarname in
					let n = getsubscript_from_v var_name in
					(* let f = fun v n -> (v,n) in
					let (_,n) = sscanf var_name "%c%d" f in *)
					sprintf "%d %s" n stmt_str
			end "" dep
			in 			
			let temp1 = sprintf "%d %s 0\n" max_rank temp1 in
			let temp2 = List.fold_left
			begin
				fun stmt_str stmt-> 
					let var_name = Sm.find stmt stmt2satvarname in
					(* let f = fun v n -> (v,n) in
					let (_,n) = sscanf var_name "%c%d" f in *)
					let n = getsubscript_from_v var_name in
					sprintf "-%d %s" n stmt_str
			end "" dep
			in
			let temp2 = sprintf "%d %s 0\n" max_rank temp2 
			in sprintf "%s%s%s" temp1 temp2 dep_str 
	end "" deplist
	in 
	let res_str = 
	List.fold_left
	begin
		fun rs (s1,s2) -> 
			let v_s1 = Sm.find s1 stmt2satvarname in
			let v_s2 = Sm.find s2 stmt2satvarname in
			let n_s1 = getsubscript_from_v v_s1 in
			let n_s2 = getsubscript_from_v v_s2 in
			let str = sprintf "%d -%d %d 0\n" max_rank n_s1 n_s2 in
			sprintf "%s%s" rs str
	end res_str dlpair_list 
	in
	let res_str = 
	Sm.fold 
	begin
		fun k v str -> 
			let f = fun v n -> (v,n) in
			let (_,n) = sscanf k "%c%d" f in (
			if is_lock_var k stmt2satvarname then sprintf "%s%d %d 0\n" str v n
			else sprintf "%s%d -%d 0\n" str v n
			)
	end rank_m res_str in	
	let clause_num = ((List.length deplist) * 2) + (var_num) + (List.length dlpair_list)in
	let res_str = sprintf "p wcnf %d %d %d\n%s" var_num clause_num max_rank res_str
	in 
	let oc = open_out "maxsat" in
	let () = fprintf oc "%s" res_str in
	let _ = close_out oc in
	printf "%s" res_str