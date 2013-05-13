open Dlgraph
open Printf

let gen_vartp_list g = 
	let rec genlist i ub = if i<ub then (String.concat "" ["v";string_of_int i])::(genlist (i+1) ub) else [] in
	let ub = g#vertex_size in
	let vstrlist = g#list_vertices_str in
	let vlist = genlist 0 ub in
	let vtplist = List.combine vstrlist vlist in vtplist 
	
let create_yices_str g paths = 
	let ret_str = "" in
	let tplist = gen_vartp_list g in
	let ret_str=List.fold_left (fun rs a-> match a with (s,v) -> let rs = sprintf "%s(define %s::bool)\n" rs v in rs) ret_str tplist in (** variable declaration **)
	ret_str
	
	
let create_yices_file g paths = 
	let oc = open_out "main.y" in
	let tplist = gen_vartp_list g in
	let _=List.iter (fun a-> match a with (s,v) -> fprintf oc "(define %s::bool)\n" (v)) tplist in (** variable declaration **)
	Vss.iter (fun p -> 	
		let varlist = Vs.fold 
			begin 
				fun elt ls  -> 
					let vstr = g#label elt in 
					printf "%s : %s \n" vstr (List.assoc vstr tplist);
					(List.assoc vstr tplist)::ls;
			end p [] 
		in 
		let () = List.iter (fun v -> printf "%s " v) varlist in
		let () = printf "\n" in
		let () = fprintf oc "(assert+ (not (and %s)) 10)\n" (String.concat " " varlist) in
		let sl = List.map (fun s-> sprintf "(not %s)" s) varlist in
		fprintf oc "(assert+ (not (and %s)) 10)\n" (String.concat " " sl)		
		) paths;
	List.iter (fun a-> fprintf oc "(assert+ %s 8)\n" (snd a)) tplist;
	fprintf oc "(max-sat)";
	close_out oc;;

(*	
let enum_paths v0 v1 g g' = 
	if (* not(g#mem_edge v0 v1)&&*)(g#mem_edge v1 v0) then
		let v0' = g'#find_string_vertex (g#label v0) in
		let v1' = g'#find_string_vertex (g#label v1) in
		if g'#mem_edge v0' v1' then
			let path = Vs.empty in
			let path = Vs.add v0 path in
			let res = Vss.empty in
			let res = g#search v0 v1 v0 3 path res in res
		else Vss.empty
	else Vss.empty	
*)

let enum_paths v0 v1 g g' = 
	if (* not(g#mem_edge v0 v1)&&*)(g#mem_edge v1 v0) then
		let v0' = g'#find_string_vertex (g#label v0) in
		let v1' = g'#find_string_vertex (g#label v1) in (*printf "%s %s\n" (g'#label v0')(g'#label v1'); g#print_all_edges;*)
		if g'#mem_edge v0' v1' then 
			let path = [v0] in (* printf "%s %s\n" (g#label v0)(g#label v1); *)
			let res = g#search v0 v1 v0 2 path [] in (*printf"res %d " (List.length res);*)res
		else []
	else []

	
(*
let persist gList = 
	let () = List.iter
	begin
		fun g-> 
		begin
		let g' = g#transitive_closure in
		let vl = g#list_vertices in
		let res = List.fold_left 
			begin
				fun rs v0-> 
					let tres = List.fold_left
					begin
						fun trs v1 -> Vss.union trs (enum_paths v0 v1 g g')						
					end Vss.empty vl 
					in Vss.union rs tres
			end Vss.empty vl
		in  create_yices_file g res
		end
	end gList
	in ()
	
	*)
(*
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
						fun v1->
						if (* not(g#mem_edge v0 v1)&&*)(g#mem_edge v1 v0) then
							let v0' = g'#find_string_vertex (g#label v0) in
							let v1' = g'#find_string_vertex (g#label v1) in
							if g'#mem_edge v0' v1' then
								let path = Vs.empty in
								let path = Vs.add v0 path in
								let res = Vss.empty in
								let res = g#search v0 v1 v0 3 path res in
								let ret_str = create_yices_str g res in printf "%s" ret_str
								(* 
									  Vss.iter (fun b -> Vs.iter (fun v -> printf "%s " (g#label v)) b;
									  printf "%d\n" (List.length (Vs.elements b))) res	*)
						end
				end
			end
	end gList
	in ()
	
	*)