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

	
let vertex_lst_to_str_lst g vlst = 
	List.fold_left
	begin
		fun strlst v -> 		
		 (g#label v)::strlst
	end
	[] vlst


let print_str_lst lst = List.iter (printf "%s ") lst

let print_str_lsts lsts = List.iter (fun lst -> print_str_lst lst; printf "\n") lsts

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
	
	
	
let list_to_set (lst : string list) : Ss.t = 
	List.fold_left	(fun res str -> Ss.add str res)
	Ss.empty lst
	
(* is l1 sublist of l2*)
let rec isSublist (l1 : string list) (l2 : string list) (inComp : bool) : bool =
	match l1,l2 with
	| x1::xl1,x2::xl2 -> 
		begin
			if x1=x2 && inComp then isSublist xl1 xl2 inComp
			else if x1=x2 && not inComp then isSublist xl1 xl2 true
			else if x1!=x2 && inComp then false
			else if x1!=x2 && not inComp  then isSublist l1 xl2 false
			else false
		end
	| [],_ -> inComp
	| _ -> false

(* is l1 a rotation to l2*)
let isRotation (l1 : string list) (l2 : string list) : bool = 
	let nl1 = l1@l1 in
	isSublist l2 nl1 false

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
	List.fold_left
	begin
		fun rl l -> 
			l::(List.filter (fun l'-> not (isRotation l' l)) rl)
	end reslist reslist 

(*string tokenizer*)
let rec split_char sep str =
  try
    let i = String.index str sep in
    String.sub str 0 i ::
      split_char sep (String.sub str (i+1) (String.length str - i - 1))
  with Not_found ->
    [str]
	
let parse_stmt_str_info (stmt:string) : (string*(string list)*string*string) = 
	let substrs = split_char ';' stmt in
	match substrs with 
	|threadFunc :: hold_locks :: opr :: acq_lock :: [] -> 
		let hold_locks_lst = split_char ',' hold_locks in
		(threadFunc,hold_locks_lst,opr,acq_lock)
	|_->("",[""],"","")

let find_lock_str (threadFunc: string) (acq_lock : string) (stmtmap : Ss.t Sm.t) : string = 
	let bindings = Sm.bindings stmtmap in
	let rec rfind_lock_str (threadFunc: string) (acq_lock : string) (bindings : (Sm.key*Ss.t) list) : string = 
		match bindings with
		| (k,_)::xbindings -> let (tf,_,_,al) = parse_stmt_str_info k in 
			if tf=threadFunc && al=acq_lock then k
			else rfind_lock_str (threadFunc) (acq_lock) (xbindings)
		| _ -> ""
	in rfind_lock_str threadFunc acq_lock bindings
	
(* count #locks need to be release in cyclic dependency *)
let rec count_release_locks (dep:string list) (stmt2lockset : Ss.t Sm.t) (res : int Sm.t) : (int Sm.t) = 
	match dep with
	| d1::d2::xdep -> 
		let (threadFunc,_,_,rel_lock) = parse_stmt_str_info d1 in 
		let roll_back_to_str = find_lock_str threadFunc rel_lock stmt2lockset in 
		if roll_back_to_str="" then 
			let res = Sm.add d2 1 res in 
			count_release_locks (d2::xdep) stmt2lockset res 
		else
			let (_,hl,_,_) = parse_stmt_str_info d2 in
			let (_,rhl,_,_) = parse_stmt_str_info roll_back_to_str in 
			(*let delta = (List.length hl) - (List.length rhl) in *)
			let lkset1 = Sm.find d2 stmt2lockset in
			let lkset2 = Sm.find roll_back_to_str stmt2lockset in
			let delta = Ss.cardinal (Ss.diff lkset1 lkset2) in
			let res = Sm.add d2 delta res in 
			count_release_locks (d2::xdep) stmt2lockset res 
	| _ -> res
	
let rec last_elem lst = 
	match lst with
	| x::[] -> x
	| x::xlst -> last_elem xlst
	
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
								fun ts graphList -> 
									begin
										hlper#reset_acq_cache;
										hlper#reset_hold_cache;
										hlper#reset_lockset;
										(hlper#marklockset_ts ts threadFunm) :: graphList
									end
							end tss []
					in
					let deplstlst = gen_unique_cyclic_lock_dep graphList
					in 
					let deplst = List.hd deplstlst in
					let deplst = (last_elem deplst) :: deplst in
					(*print_str_lsts deplstlst;*)
					let m = 
					count_release_locks (deplst) (hlper#get_stmt2lockset) (Sm.empty) in
					printf "%d\n" (Sm.cardinal m);
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
visitCilFileSameGlobals (new mainVisitor) f
