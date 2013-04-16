open Cil
open Printf
open List
open Cfg
open Set

type threadT = { fname : string; }

module ThreadMod = struct
	type t = threadT
	let compare = Pervasives.compare
end

module Ts = Set.Make(ThreadMod)
module Tss = Set.Make(Ts)

let print_ts ts = 
	Ts.iter 
		begin
			fun eelt -> printf "%s " eelt.fname
		end
		ts

let print_tss tss = 
		Tss.iter 
			begin
				fun elt -> 
				print_ts elt;
				printf "\n"
			end
		tss;
		printf"-----------\n"
		
		
let ctc_instr il tss = 
	let ilset = Ts.empty in
	let ilset = List.fold_left 
		begin
			fun ilset a -> 
			match a with
			| Call(_,Lval(lv),a1::a2::AddrOf(arg)::arglst,_) -> 
				begin
					match ((fst lv),(fst arg)) with 
					| (Var(varinfo),Var(arginfo)) -> 
						if varinfo.vname = "pthread_create" 
							then let nthrd = {fname = arginfo.vname} 
								in 
								Ts.add nthrd ilset
						else 
								ilset
					| _ -> ilset
				end
			|_ -> ilset
		end 
		ilset il
	in
	if Ts.is_empty ilset then tss (* ignore instr which is not pthread_create *)
	else 
		let ret_tss = Tss.empty in
		let ret_tss = 
			if Tss.is_empty tss
				then Tss.add ilset ret_tss
			else
				Tss.fold 
				begin 
					fun elt ret_tss -> 
						if(Ts.is_empty elt) then ret_tss
						else Tss.add (Ts.union elt ilset) ret_tss
				end
				tss ret_tss 
		in	
		ret_tss
	
let rec ctc_if tpath fpath tss = 
	let ttss = Tss.empty in
	let ttss = List.fold_left
			begin
				fun ttss elt -> 
					let ntss = 
						match elt.skind with
						Instr(il)-> ctc_instr il tss
						| If(_,tp,fp,_) -> ctc_if tp fp tss
						| _ -> tss
					in Tss.union ntss tss
			end
		ttss tpath.bstmts
	in let ftss = Tss.empty in
	let ftss = List.fold_left
			begin
				fun ftss elt -> 
					let ntss = 
						match elt.skind with
						Instr(il)-> ctc_instr il tss
						| If(_,tp,fp,_) -> ctc_if tp fp tss
						| _ -> tss
					in Tss.union ntss tss
			end
		ftss fpath.bstmts
	in 
	Tss.union ttss ftss

let rec collect_thread_create stmts tss = 
	match stmts with
	s::xstmts 
		-> let ntss =   
			match s.skind with
			| Instr(il) -> ctc_instr il tss
			| If(_,tpath,fpath,_) -> ctc_if tpath fpath tss
			| _ -> tss
			in collect_thread_create xstmts ntss 
	| _ -> tss		

class mainVisitor = object
	inherit nopCilVisitor
	
	method vfunc (f : fundec) : fundec visitAction = 
		if f.svar.vname = "main" 
			then (					
					let tss = Tss.empty in
					let tss = collect_thread_create f.sbody.bstmts tss in 	
						 print_tss tss; 
					SkipChildren;
				)
		else SkipChildren;
end

let f = Frontc.parse "deadlock.cil.c" ();;

visitCilFileSameGlobals (new mainVisitor) f