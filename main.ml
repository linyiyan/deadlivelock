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
module VertexSet = Set.Make(StringDigraph.V) 
module VertexSetSet = Set.Make(VertexSet) 
module OP = Oper.I(StringDigraph)

let f=Frontc.parse "deadlock.cil.c" () ;;
computeFileCFG f ;;

let rec globalFundec gf = 
	match gf with
	GFun(fundec , _)::xgf -> fundec :: globalFundec xgf
	| _::xgf -> globalFundec xgf
	| _ -> [] ;;
	
let create_str_vertex g str vertex_cache = 
	if StringMap.mem str vertex_cache then (StringMap.find str vertex_cache),vertex_cache
	else let v=StringDigraph.V.create str in
		 let vertex_cache = StringMap.add str v vertex_cache in
		 v,vertex_cache
	

let rec markMutexLock instrl lockset acq_cache hold_cache vertex_cache locksetg= 
	match instrl with
	Call(_,Lval(lvar),AddrOf(arg)::lst,_)::iistrl
		-> (
			match ((fst lvar),(fst arg)) with 
			(Var(varinfo),Var(arginfo)) ->
			begin
				printf "Lval: %s %s \n" varinfo.vname arginfo.vname; 
				let acq_cache,hold_cache,vertex_cache
					= 
					if varinfo.vname = "pthread_mutex_lock" then
						begin 
							if StringSet.is_empty lockset then acq_cache,hold_cache,vertex_cache
							else let acqstr = String.concat "," (StringSet.elements lockset)
								 in let acqstr = String.concat ";" [acqstr;"mutex_lock";arginfo.vname]
								 in (
									printf "acqstr: %s\n" acqstr;
									let vertex_cache  
										= 
										if StringMap.mem arginfo.vname hold_cache then 
										let s = StringMap.find arginfo.vname hold_cache in
										let v1,vertex_cache = create_str_vertex locksetg s vertex_cache in
										let v0,vertex_cache = create_str_vertex locksetg acqstr vertex_cache in
											begin
												StringDigraph.add_edge locksetg v0 v1;
												vertex_cache;
											end
									else (vertex_cache) 
									in
									let vertex_cache 
										= StringSet.fold (
										fun hold cache -> 
											if StringMap.mem hold acq_cache then 
												begin
													let s  = StringMap.find hold acq_cache in
													let v1,vertex_cache = create_str_vertex locksetg acqstr vertex_cache in
													let v0,vertex_cache = create_str_vertex locksetg s vertex_cache in
														begin
															StringDigraph.add_edge locksetg v0 v1;
															vertex_cache;
														end
												end
											else (vertex_cache)
									) lockset vertex_cache
									in 
									StringMap.add arginfo.vname acqstr acq_cache , 
									StringSet.fold (fun s m -> StringMap.add s acqstr m) lockset hold_cache , 
									vertex_cache;
									)
						end
					else acq_cache,hold_cache,vertex_cache
				in 
				let lockset 
					= 
					if varinfo.vname = "pthread_mutex_lock" then 
							StringSet.add arginfo.vname lockset					
				    else if varinfo.vname = "pthread_mutex_unlock" then 
						StringSet.remove arginfo.vname lockset
				    else lockset
				in	
					
					markMutexLock iistrl lockset acq_cache hold_cache vertex_cache locksetg;
			end
			|_-> (markMutexLock iistrl lockset acq_cache hold_cache vertex_cache locksetg;)
		   )
	|_::iistrl -> (markMutexLock iistrl lockset acq_cache hold_cache vertex_cache locksetg)
	|_ -> (lockset,acq_cache,hold_cache,vertex_cache,locksetg)
	
	
let rec processStmt stmts lockset acq_cache hold_cache vertex_cache locksetg= 
	match stmts with
	s::xstmts -> 
		begin 
			match s.skind with
			| Instr(instrl) 
			   -> (let lockset,acq_cache,hold_cache,vertex_cache,locksetg 
						= markMutexLock instrl lockset acq_cache hold_cache vertex_cache locksetg
				   in let acq_cache,hold_cache,vertex_cache,locksetg 
						= processStmt s.succs lockset acq_cache hold_cache vertex_cache locksetg 
				   in acq_cache,hold_cache,vertex_cache,locksetg)
			|_ -> (let acq_cache,hold_cache,vertex_cache,locksetg 
						= processStmt s.succs lockset acq_cache hold_cache vertex_cache locksetg
				   in acq_cache,hold_cache,vertex_cache,locksetg)
		end
	| _ -> acq_cache,hold_cache,vertex_cache,locksetg;;
	
exception Found of StringDigraph.V.t
let find_string_vertex g i =
    try
      StringDigraph.iter_vertex (fun v -> if StringDigraph.V.label v = i then raise (Found v)) g;
      raise Not_found
    with Found v -> v;;
	
let rec search g src tgt cur k path res= 
	if(k>0 && tgt=cur) then (VertexSetSet.add path res)
	else if (k<=0) then (VertexSetSet.empty)
	else (
	let succlst = StringDigraph.succ g cur in
	List.fold_left (fun res sc -> 
		let path = VertexSet.add sc path in
		search g src tgt sc (k-1) path res) res succlst
	);;
	

	
let create_yices_file paths = 
	let oc = open_out "main.y" in
	VertexSetSet.iter (fun b -> 
		( VertexSet.iter (fun a-> let a = StringDigraph.V.label a in fprintf oc "%s \n" a))  b; (** variable declaration **)
		) paths;
	close_out oc;;

let funl = globalFundec f.globals in
let rec processFuns fl acq_cache hold_cache vertex_cache locksetg= 
	let lockset = StringSet.empty in (** Assume lockset is empty on each entry of thread function **)	
	match fl with
		f::xfl -> (let acq_cache,hold_cache,vertex_cache,locksetg 
						= processStmt f.sallstmts lockset acq_cache hold_cache vertex_cache locksetg
					in let acq_cache,hold_cache,locksetg 
						= processFuns xfl acq_cache hold_cache vertex_cache locksetg 
					in acq_cache,hold_cache,locksetg)
		| _-> acq_cache,hold_cache,locksetg
	in
		let ac=StringMap.empty 
		in let hc=StringMap.empty 
		in let lg= StringDigraph.create() 
		in let vc = StringMap.empty   (** Cache all the vertex in avoidance of duplicate by label **)
		in 
		let ac,hc,lg = processFuns funl ac hc vc lg
		in 
			begin
				let lg' = OP.transitive_closure ~reflexive:false lg in					
				StringDigraph.iter_vertex 
					(fun v0 -> 
						StringDigraph.iter_vertex 
							(fun v1 -> 
								if (not (StringDigraph.mem_edge lg v0 v1)) && (StringDigraph.mem_edge lg v1 v0)
									then 
									let v0' = find_string_vertex lg' (StringDigraph.V.label v0) in
									let v1' = find_string_vertex lg' (StringDigraph.V.label v1) in
									if StringDigraph.mem_edge lg' v0' v1' then (** cycle between v0 and v1 exists in original graph *)
										let path = VertexSet.empty in
										let path = VertexSet.add v0 path in
										let res = VertexSetSet.empty in
										let res = search lg v0 v1 v0 3 path res in
										create_yices_file res
									else ()
								else ()
							) lg
					) lg
			end
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			