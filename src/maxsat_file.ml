open Map
open Printf
open Scanf
module Sm = Map.Make(String)

let gen_maxsat_file (stmt2satvarname : string Sm.t) (deplist : string list list) (rank_m : int Sm.t) =
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
					let f = fun v n -> (v,n) in
					let (_,n) = sscanf var_name "%c%d" f in 
					sprintf "%d %s" n stmt_str
			end dep_str dep
			in 
			
			let temp1 = sprintf "%d %s 0\n" max_rank temp1 in
			let temp2 = List.fold_left
			begin
				fun stmt_str stmt-> 
					let var_name = Sm.find stmt stmt2satvarname in
					let f = fun v n -> (v,n) in
					let (_,n) = sscanf var_name "%c%d" f in
					sprintf "-%d %s" n stmt_str
			end dep_str dep
			in
			let temp2 = sprintf "%d %s 0\n" max_rank temp2 
			in sprintf "%s%s%s" temp1 temp2 dep_str 
	end "" deplist
	in 
	let res_str = 
	Sm.fold 
	begin
		fun k v str -> 
			let f = fun v n -> (v,n) in
			let (_,n) = sscanf k "%c%d" f in 
			sprintf "%s%d %d 0\n" str v n 
	end rank_m res_str in	
	let clause_num = ((List.length deplist) * 2) + (var_num) in
	let res_str = sprintf "p wcnf %d %d %d\n%s" var_num clause_num max_rank res_str
	in 
	let oc = open_out "maxsat" in
	let () = fprintf oc "%s" res_str in
	let _ = close_out oc in
	printf "%s" res_str