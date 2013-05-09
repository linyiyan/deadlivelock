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
	
let rec get_prev_elt (elt:string) (lst : string list) : string= 
	match lst with
	| e1::e2::xlst -> if elt=e2 then e1 else get_prev_elt elt (e2::xlst)
	| _ -> ""
	

class unlock_helper (unlock_lock:string) (stmt:string) (func : fundec) = object (self)
	val mutable lockset = Ss.empty
	val mutable stmt_found = false
	val mutable tgt_lval = ""
	
	method is_lock_opr opr = opr="pthread_mutex_lock" || opr = "pthread_mutex_trylock"
	
	method ps_il (il : instr list) : bool  = 
		let (_,lk_lst , _opr , lk) = parse_stmt_str_info stmt in
		match il with
		| Call(Some(retval,_),Lval(lvar),AddrOf(arg)::lst,loc):: xil -> 
			let retval_name,opr,acq = 
			begin
				match ((retval),(fst lvar),(fst arg)) with 
				| (Var(ret_varinfo),Var(varinfo),Var(arginfo)) ->ret_varinfo.vname,varinfo.vname,arginfo.vname
				| _ -> "","",""
			end
			in 			
			if lk=acq && (List.for_all (fun lk -> Ss.mem lk lockset) lk_lst) then let ()=tgt_lval<-retval_name in true
			else 
				let () = 
					if (self#is_lock_opr opr) then	
						lockset<-Ss.add acq lockset (* update lockset *)					
					else if opr = "pthread_mutex_unlock" then lockset<-Ss.remove acq lockset
				in
				self#ps_il xil
		| _ :: xil -> self#ps_il xil
		| _ -> false
		
	method ps_if (tp:block) (fp:block) : bool = 
		if not stmt_found then 
			self#ps_stmts (tp.bstmts) || self#ps_stmts (fp.bstmts)		
		else false
			
	
	method ps_stmts (stmts : stmt list) : bool = 
		match stmts with
		| st::xsts -> 
			begin
				match st.skind with
				| Instr(il) -> 
					if self#ps_il (il) 
						then stmt_found<- true ; 
					self#ps_stmts xsts
				| If (exp,tp,fp,_) ->  
					begin
						if(stmt_found) then
						match exp with
						| BinOp(Eq,Lval(Var(varinfo),_) ,e2 , _) -> if(varinfo.vname=tgt_lval) then (self#contains_unlock_stmt unlock_lock fp.bstmts) else (self#ps_if tp fp; self#ps_stmts xsts;) 
						| _ -> ( self#ps_if tp fp; self#ps_stmts xsts;)
						else 
							( self#ps_if tp fp; 
							self#ps_stmts xsts;)
					end
				| Loop(blk,_,_,_) -> self#ps_stmts blk.bstmts; self#ps_stmts xsts
				|_ -> self#ps_stmts xsts
			end
		| [] -> false
		
		
	method contains_unlock_il (unlock_lock:string) (il : instr list) : bool = 
		List.fold_left
		begin
			fun brs ins ->
				match ins with
					| Call(_,Lval(lvar),AddrOf(arg)::lst,_) ->  
						let opr,acq = 
						begin
							match ((fst lvar),(fst arg)) with 
							| (Var(varinfo),Var(arginfo)) ->varinfo.vname,arginfo.vname
							| _ -> "",""
						end
						in if opr="pthread_mutex_unlock" && acq=unlock_lock then (printf "unlock reachable" ; true) else (printf "unlock not reachable" ; false;)
					| _ -> brs=brs || brs
		end false il
	
		
	method contains_unlock_stmt (unlock_lock:string) (stmts : stmt list): bool = 
		match stmts with
		| st::xsts -> 
			begin
				match st.skind with
				| Instr(il) -> self#contains_unlock_il unlock_lock il
				| _ -> self#contains_unlock_stmt unlock_lock xsts
			end
		| [] -> false
		
	method is_unlock_reachable : bool = 
		let bstmts = func.sbody.bstmts in false
end;;

	
let is_unlock_reachable (unlock_lock : string) (stmt : string) (func : fundec) : bool = 
	let bstmts = func.sbody.bstmts in (new unlock_helper unlock_lock stmt func)#ps_stmts bstmts
	

let is_deadlivelock_pair (prev_stmt : string) (stmt : string) (func : fundec) : bool = 
	let (_,_,opr,_) = parse_stmt_str_info stmt in if not (opr="pthread_mutex_trylock") then (false)
	else
	let (_,_,_,unlock_lock) = parse_stmt_str_info prev_stmt in 	
	not (is_unlock_reachable (unlock_lock) (stmt) (func))

let compute_deadlivelock_pairs (func : fundec) (dep : string list) : (string * string) list = 
	List.fold_left
		begin
			fun rs stmt -> 
			let prev_stmt_in_dep = get_prev_elt stmt ((last_elem dep)::dep) in  
				if is_deadlivelock_pair prev_stmt_in_dep stmt func then 
				(printf "%s,%s\n" prev_stmt_in_dep stmt;
				(prev_stmt_in_dep,stmt)::rs)
				else rs
		end	[] dep
	

	
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
					let deplstlst = gen_unique_cyclic_lock_dep graphList
					in 
					let deplst = List.hd deplstlst in
					let deplst = (last_elem deplst) :: deplst in
					let deplst = List.tl deplst in
					print_str_lsts deplstlst;
					let m = 
					count_release_locks (deplst) (hlper#get_stmt2lockset) (Sm.empty) in
					List.iter 
						begin
							fun stmt -> 
								let (tf,_,_,_)=parse_stmt_str_info stmt in 
								let threadFuncdec = Sm.find tf threadFunm in
								compute_deadlivelock_pairs threadFuncdec deplst;
								();
						end deplst;
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
visitCilFileSameGlobals (new mainVisitor f) f
