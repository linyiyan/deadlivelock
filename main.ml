open Cil
open List
open Cfg
open Printf
let f=Frontc.parse "deadlock.cil.c" ();;
computeFileCFG f;;

let isMutexGvar gv = 
	match gv with
	GVar(varinfo , initinfo , location) -> 	(match varinfo.vtype with											
											| TNamed(typeinfo,_) -> true
											| _ -> false)
	|_ -> false;;
let mutexl = List.filter (isMutexGvar) f.globals;;


let rec globalFundec gf = 
	match gf with
	GFun(fundec , _)::xgf -> fundec :: globalFundec xgf
	| _::xgf -> globalFundec xgf
	| _ -> [];;
let funl = globalFundec f.globals;;

let rec markMutexLock instrl = 
	match instrl with
	i::iistrl -> (match i with
					| Call(_,exp,_,_) -> (*(printf "Call\n" ; markMutexLock iistrl;)*)(match exp with
											AddrOf(_) -> (printf "AddrOf\n" ; markMutexLock iistrl;)
											|AddrOfLabel(_) -> (printf "AddrOfLabel\n" ; markMutexLock iistrl;)
											|CastE(_,_) -> (printf "CastE\n" ; markMutexLock iistrl;)
											|Lval (lvar) -> (printf "Lval: %s\n" (match (fst lvar) with 
																					Var(varinfo) -> varinfo.vname
																					| _ -> "") ; markMutexLock iistrl;)
											| _ -> (markMutexLock iistrl;))
					| Set (_,_,_) -> (printf "Set\n" ; markMutexLock iistrl;)
					| _ -> (markMutexLock iistrl;))
	|_ -> ();;

let rec processStmt stmts = 
	match stmts with
	s::xstmts -> begin 
					match s.skind with
				  	| Instr(instrl) -> (printf "Instr %d\n" (List.length instrl); markMutexLock instrl ;processStmt s.succs;)
				  	| Return(_,_) -> (printf "Return\n"; processStmt s.succs;)
				  	| Goto (_,_) -> (printf "Goto\n"; processStmt s.succs;)
				  	| ComputedGoto (_,_) -> (printf "ComputedGoto\n"; processStmt s.succs;)
				  	| Break (_) -> (printf "Break\n"; processStmt s.succs;)
				  	| Continue (_) ->  (printf "Continue\n"; processStmt s.succs;)
				  	| If (_,_,_,_) -> (printf "If\n"; processStmt s.succs;)
				  	| Switch (_,_,_,_) -> (printf "Switch\n"; processStmt s.succs;)
				  	| Loop (_,_,_,_) -> (printf "Loop\n"; processStmt s.succs;)
				  	| Block (_) -> (printf "Block\n"; processStmt s.succs;)
				  	| TryFinally (_,_,_) -> (printf "TryFinally\n"; processStmt s.succs;)
				  	| TryExcept (_,_,_,_) -> (printf "TryExcept\n"; processStmt s.succs;)
				end
	| _ -> ();;

processStmt (hd funl).sallstmts;;

(* List.iter (processStmt) funl;; *)
	


let print_main_cfg = function
	GFun(fundec , location) -> if(fundec.svar.vname="main") then printCfgFilename "dumbout" fundec
	|_ -> ();;

List.iter (print_main_cfg) f.globals;;
