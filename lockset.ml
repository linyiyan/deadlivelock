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
			let opr,acq = 
			begin
				match ((fst lvar),(fst arg)) with 
				| (Var(varinfo),Var(arginfo)) -> varinfo.vname,arginfo.vname
				| _ -> "",""
			end
			in
			let locksetg = (* acq vertex -->  hold cache *)
			begin				
				if opr = "pthread_mutex_lock" then
					begin						
						let acqstr = String.concat "," (Ss.elements lockset)
						in let acqstr = String.concat ";" [acqstr;"mutex_lock";acq] in
						printf "acqstr: %s\n" acqstr;
						if Sm.mem acq hold_cache then							
							let ss = Sm.find acq hold_cache in
							let () = Ss.iter (fun s -> locksetg#add_str_edge acqstr s; ()) ss
							in locksetg
						else locksetg 
					end
				else locksetg
			end
			in
			let locksetg = (* hold vertices <-- acq cache *)
			begin
				if opr = "pthread_mutex_lock" then 
					begin
						Ss.iter 
							(fun hold -> 
								let acq_strs = if Sm.mem hold acq_cache then Sm.find hold acq_cache else Ss.empty
								in 
								Ss.iter (fun acq_str -> locksetg#add_str_edge hold acq_str; ()) acq_strs
							) 
						lockset ;
					locksetg;
					end
					
				else locksetg 
			end
			in
			let () = (* update lockset *)
				if opr = "pthread_mutex_lock" then	lockset<-Ss.add acq lockset		
				else if opr = "pthread_mutex_unlock" then lockset<-Ss.remove acq lockset
				else ()			
			in
			self#ml_il xil locksetg
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
		
