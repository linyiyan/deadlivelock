open Dlgraph
open Map
open Set
open Dlutil
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
						(* if (List.length vlsts) > 0 then (printf "enum paths: ";print_str_lsts strlsts; printf "\n") else ();
						print_str_lsts;
						printf "\n"; *)
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
	(* let () = List.iter (fun g-> g#print_all_edges ; printf "\n ------------- \n") gList in *)
	let deplist = gen_cyclic_lock_dep gList in (* printf "dependency list\n" ; print_str_lsts deplist; printf "\n"; *)
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