open Cil
open Set
open Printf
open Dlutil
module Ss = Set.Make(String)
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
	

let is_deadlivelock_pair (prev_stmt : string) (stmt : string) (threadFunm : fundec Sm.t) : bool = 
	let (_,_,opr,_) = parse_stmt_str_info stmt in if not (opr="pthread_mutex_trylock") then (false)
	else
	let (func_name,_,_,unlock_lock) = parse_stmt_str_info prev_stmt in 	
	not (is_unlock_reachable (unlock_lock) (stmt) (Sm.find func_name threadFunm))


let compute_deadlivelock_pairs (dep : string list) (threadFunm : fundec Sm.t): (string * string) list = 
	List.fold_left
		begin
			fun rs stmt -> 
			let prev_stmt_in_dep = get_prev_elt stmt ((last_elem dep)::dep) in  
				if is_deadlivelock_pair prev_stmt_in_dep stmt threadFunm then 
				(printf "%s,%s\n" prev_stmt_in_dep stmt;
				(prev_stmt_in_dep,stmt)::rs)
				else rs
		end	[] dep
(*	
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
*)
	