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
module Ss = Set.Make(String)
module Sss = Set.Make(Ss)

(*
let gen_cyclic_lock_dep gList = 
	let depset = List.fold_left
	begin
		fun depset g-> 
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
		in  Vss.union res depset
		end 
	end Vss.empty gList
	in depset *)
	
let vertex_lst_to_str_lst g vlst = 
	List.fold_left
	begin
		fun strlst v -> 
		 (g#label v)::strlst
	end
	[] vlst
	(*
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
						let vlst = enum_paths v0 v1 g g' in
						let strlst = vertex_lst_to_str_lst g vlst in
						List.append trs strlst					
					end [] vl 
					in List.append rs tres
			end [] vl
		in  List.append res deplist
		end 
	end [] gList
	in deplist	
*)




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
									strlst :: strlsts
							end [] vlsts in
						strlsts :: trs 
					end [] vl 
					in tres @ rs
			end [] vl
		in  res @ deplist
		end 
	end [] gList
	in deplist	
	
	
	
let list_to_set (lst : string list) : Ss.t = 
	List.fold_left	(fun res str -> Ss.add str res)
	Ss.empty lst

(*let gen_unique_cyclic_lock_dep gList = 
	let deplist = gen_cyclic_lock_dep gList in
	let depset = Sss.empty in 
	let reslist = List.fold_left 
		begin
			fun reslist dep -> 
				let dep_s = list_to_set dep in
				if Sss.mem dep_s depset then reslist
				else 
				begin
					(* Sss.add dep_s depset; *)
					dep::reslist
				end
		end [] deplist
	in reslist*)

let gen_unique_cyclic_lock_dep gList = 
	let deplist = gen_cyclic_lock_dep gList in 
	List.iter 
	begin
		fun dep -> 
			begin
			List.iter 
			begin
				fun d -> printf "%s " d
			end;	
			printf "\n";
			end
	end deplist
	(*let depset = Sss.empty in
	let reslist = List.fold_left
		begin
			fun rl dep -> 
				let dep_s = (list_to_set dep) in
				if Sss.mem dep_s depset then rl
				else
				begin
					Sss.add dep_s depset;
					dep::rl;
				end
		end [] deplist
	in reslist *)

	
(* let gen_unique_cyclic_lock_dep gList = 
	let deplist = gen_cyclic_lock_dep gList in 
	let dep = List.hd deplist in
	let dep_s = (list_to_set dep) in [] *)

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
					(*let graphList = marklockset_tss tss threadFunm in
					let ()= persist graphList in 	*)
					let hlper = new locksetHelper in
					let graphList = 
						Tss.fold 
							begin
								fun ts graphList -> (hlper#marklockset_ts ts threadFunm) :: graphList
							end tss []
					in
					let () = gen_unique_cyclic_lock_dep graphList
					in 
					DoChildren;
				end
		else 
			begin
				threadFunm <- Sm.add f.svar.vname f threadFunm; 
				DoChildren;
			end
end ;;


let fstr = Array.get Sys.argv 1 in
let commstr = sprintf "cilly --save-temps -D HAPPY_MOOD example/simple/%s.c -lpthread" fstr in
let fstr = sprintf "%s.cil.c" fstr in
let _ = Sys.command commstr in
let f = (Frontc.parse fstr) () in
visitCilFileSameGlobals (new mainVisitor) f
