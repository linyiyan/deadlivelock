open Printf
open Set
open Cil
open Dlgraph
open Dlthread
open List

module Sm = Map.Make(String)
module Ss = Set.Make(String)



class locksetHelper = 
	object (self)
	
	val mutable acq_cache = Sm.empty
	val mutable hold_cache = Sm.empty
	val mutable lockset = Ss.empty
	
	method ml_il il locksetg = 
		match il with
		Call(_,Lval(lvar),AddrOf(arg)::lst,_)::xil -> 
			let locksetg = 
			begin
				match ((fst lvar),(fst arg)) with 
				| (Var(varinfo),Var(arginfo)) ->
					if varinfo.vname = "pthread_mutex_lock" then
						begin
							if Ss.is_empty lockset then 
								begin 
									lockset<-Ss.add arginfo.vname lockset; 
									locksetg 
								end
							else 
								let acqstr = String.concat "," (Ss.elements lockset)
								in let acqstr = String.concat ";" [acqstr;"mutex_lock";arginfo.vname] in
								locksetg
								(* printf "acqstr: %s\n" acqstr;
								if Sm.mem arginfo.vname hold_cache then
									let s = Sm.find arginfo.vname hold_cache in
										locksetg#add_str_edge acqstr s
								else locksetg *)
						end
					else if varinfo.vname = "pthread_mutex_unlock" then
						begin
						lockset<-Ss.remove arginfo.vname lockset;
						locksetg
						end
					else locksetg
				| _ -> locksetg
			end
			in self#ml_il xil locksetg
		| _::xil -> self#ml_il xil locksetg
		| _-> locksetg
		

	method ml_if tpath fpath locksetg = locksetg
	
	method marklockset stmts locksetg = 
		match stmts with
		s::xstmts -> 
			let locksetg = 
				begin
					match s.skind with
					| Instr(il) -> self#ml_il il locksetg
					| If(_,tp,fp,_) ->  self#ml_if tp fp locksetg
					| _ -> locksetg
				end
			in (self)#marklockset xstmts locksetg
		| _ -> locksetg
	
	method marklockset_ts ts tfm = 
		let locksetg = new lockGraph in
		Ts.fold 
			begin 
				fun elt locksetg
					-> 
					let fundec = Sm.find elt.fname tfm in 
						self#marklockset fundec.sbody.bstmts locksetg
			end ts locksetg
end 			


let marklockset_tss tss tfm = 
	let graphList = [] in
	Tss.fold 
		begin
			fun ts graphList -> ((new locksetHelper)#marklockset_ts ts tfm) :: graphList
		end tss graphList
		
