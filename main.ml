open Cil
open List
open Cfg
open Printf
open Set
open Graph
open Map
open Hashtbl
open Array
open String

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)
module StringDigraph = Imperative.Digraph.Abstract(String)
module OP = Oper.I(StringDigraph)

let f=Frontc.parse "deadlock.cil.c" () ;;
computeFileCFG f ;;

let rec globalFundec gf = 
	match gf with
	GFun(fundec , _)::xgf -> fundec :: globalFundec xgf
	| _::xgf -> globalFundec xgf
	| _ -> [] ;;

let rec markMutexLock instrl lockset acq_cache hold_cache locksetg= 
	match instrl with
	Call(_,Lval(lvar),AddrOf(arg)::lst,_)::iistrl
		-> (
			match ((fst lvar),(fst arg)) with 
			(Var(varinfo),Var(arginfo)) ->
			begin
				printf "Lval: %s %s \n" varinfo.vname arginfo.vname; 
				let acq_cache,hold_cache 
					= 
					if varinfo.vname = "pthread_mutex_lock" then
						begin 
							if StringSet.is_empty lockset then acq_cache,hold_cache
							else let acqstr = String.concat "," (StringSet.elements lockset)
								 in let acqstr = String.concat ";" [acqstr;"mutex_lock";arginfo.vname]
								 in (
									printf "acqstr: %s\n" acqstr;
									if StringMap.mem arginfo.vname hold_cache then 
										let s = StringMap.find arginfo.vname hold_cache in
										let v1 = StringDigraph.V.create s in
										let v0 = StringDigraph.V.create acqstr in
											begin
												StringDigraph.add_edge locksetg v0 v1
											end
									else ();
									StringSet.iter (fun hold -> 
													if StringMap.mem hold acq_cache then 
														begin
															let s  = StringMap.find hold acq_cache in
															let v1 = StringDigraph.V.create acqstr in
															let v0 = StringDigraph.V.create s in
															StringDigraph.add_edge locksetg v0 v1
														end
													else ()
													) lockset;
									StringMap.add arginfo.vname acqstr acq_cache , StringSet.fold (fun s m -> StringMap.add s acqstr m) lockset hold_cache;
									)
						end
					else acq_cache,hold_cache
				in 
				let lockset 
					= 
					if varinfo.vname = "pthread_mutex_lock" then 
							StringSet.add arginfo.vname lockset					
				    else if varinfo.vname = "pthread_mutex_unlock" then 
						StringSet.remove arginfo.vname lockset
				    else lockset
				in	
					
					markMutexLock iistrl lockset acq_cache hold_cache locksetg;
			end
			|_-> (markMutexLock iistrl lockset acq_cache hold_cache locksetg;)
		   )
	|_::iistrl -> (markMutexLock iistrl lockset acq_cache hold_cache locksetg)
	|_ -> (lockset,acq_cache,hold_cache,locksetg)
	
	
let rec processStmt stmts lockset acq_cache hold_cache locksetg= 
	match stmts with
	s::xstmts -> 
		begin 
			match s.skind with
			| Instr(instrl) 
			   -> (let lockset,acq_cache,hold_cache,locksetg 
						= markMutexLock instrl lockset acq_cache hold_cache locksetg
				   in let acq_cache,hold_cache,locksetg 
						= processStmt s.succs lockset acq_cache hold_cache locksetg 
				   in acq_cache,hold_cache,locksetg)
			|_ -> (let acq_cache,hold_cache,locksetg 
						= processStmt s.succs lockset acq_cache hold_cache locksetg
				   in acq_cache,hold_cache,locksetg)
		end
	| _ -> acq_cache,hold_cache,locksetg;;

let funl = globalFundec f.globals in
let rec processFuns fl acq_cache hold_cache locksetg= 
	let lockset = StringSet.empty in (** Assume lockset is empty on each entry of thread function **)
	match fl with
		f::xfl -> (let acq_cache,hold_cache,locksetg 
						= processStmt f.sallstmts lockset acq_cache hold_cache locksetg
					in let acq_cache,hold_cache,locksetg 
						= processFuns xfl acq_cache hold_cache locksetg 
					in acq_cache,hold_cache,locksetg)
		| _-> acq_cache,hold_cache,locksetg
	in
		let ac=StringMap.empty 
		in let hc=StringMap.empty 
		in let lg= StringDigraph.create() 
		in 
		let ac,hc,lg = processFuns funl ac hc lg
		in 
			begin
				let lg' = OP.transitive_closure ~reflexive:false lg in 
					printf "transitive closure edge size %d\n" (StringDigraph.nb_edges lg');
				ac,hc,lg;
			end