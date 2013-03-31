open Cil
open List
open Cfg
open Printf
open Set
open Graph

(* open Graph.Pack.Digraph *)

module StringSet = Set.Make(String) ;;
module StringDigraph = Imperative.Digraph.Abstract(String);; 

(*module Digraph = Graph.Pack.Digraph ;; *)

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

let emptyset = StringSet.empty;;
let locksetg = StringDigraph.create();;

let rec extractmtxvertex mtxl = 
	match mtxl with
	 GVar(varinfo , _,_)::xmtxl -> StringDigraph.V.create varinfo.vname::extractmtxvertex xmtxl
	| _ -> []
in let mtxvertexl = extractmtxvertex mutexl 
in List.iter (StringDigraph.add_vertex locksetg ) mtxvertexl;;

(* printf "vertex size %d\n" (StringDigraph.nb_vertex locksetg);; *)


(*
let rec create_vertex lst = 
	match lst with
	GVar(varinfo,_,_)::xlst -> ((Digraph.add_vertex lockgraph 1); create_vertex xlst;)
	| _ -> [];;

 let vertex_list = create_vertex mutexl;; *)

let rec markMutexLock instrl lockset= 
	match instrl with
	i::iistrl -> (match i with
					| Call(_,Lval(lvar),AddrOf(arg)::lst,_) -> 
						(match ((fst lvar),(fst arg)) with 
							(Var(varinfo),Var(arginfo)) -> 
								begin 
									printf "Lval: %s %s " varinfo.vname arginfo.vname; 
									let newlockset = (
										if varinfo.vname = "pthread_mutex_lock" then (
											StringSet.add arginfo.vname lockset;
											)
										else if varinfo.vname = "pthread_mutex_unlock" then StringSet.remove arginfo.vname lockset
										else lockset
									)
									in(
										List.iter (print_string) (StringSet.elements newlockset);	
										printf "\n";									
										markMutexLock iistrl newlockset;										
									)									
								end
							| _ -> ())
					| Set (_,_,_) -> (printf "Set\n" ; markMutexLock iistrl lockset;)
					| _ -> (markMutexLock iistrl lockset;))
	|_ -> ();;

let rec processStmt stmts = 
	match stmts with
	s::xstmts -> begin 
					match s.skind with
				  	| Instr(instrl) -> (printf "Instr %d\n" (List.length instrl); markMutexLock instrl emptyset ;processStmt s.succs;)
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
