open Cil
open List
open Cfg
open Printf
open Set
open Graph
open Map
(* open Graph.Pack.Digraph *)

module StringSet = Set.Make(String) ;;
module StringDigraph = Imperative.Digraph.Abstract(String);; 
module OP = Oper.I(StringDigraph);;
module Tra = Traverse.Bfs(StringDigraph);;
module StringMap = Map.Make(String);;
module VertexSet = Set.Make(StringDigraph.V) ;;
module VertexSetSet = Set.Make(VertexSet) ;;
module EdgeMap = Map.Make(StringDigraph.E);;

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
let edge2acq = EdgeMap.empty;;
(*let str2vertexmap = StringMap.empty;; *)

(*
let buildStringMap g str = 
let v = StringDigraph.V.create str
in let str2vertexmap = StringMap.add str v str2vertexmap in ( 
StringMap.find str str2vertexmap;
StringDigraph.add_vertex g v) ;;



let rec extractmtxvertex mtxl = 
	match mtxl with
	 GVar(varinfo , _,_)::xmtxl -> varinfo.vname::extractmtxvertex xmtxl;
	| _ -> []
in let mtxvertexl = extractmtxvertex mutexl 
in List.iter (buildStringMap locksetg) mtxvertexl;;
*)

let rec extractmtxvertex mtxl = 
	match mtxl with
	 GVar(varinfo,_,_)::xmtxl -> (varinfo.vname , StringDigraph.V.create varinfo.vname)::extractmtxvertex xmtxl;
	| _ -> [];;
let mtxvertexl = extractmtxvertex mutexl;;

let str2vertexmap = List.fold_left (fun str2vertexmap (str, v) -> 
	StringMap.add str v str2vertexmap) StringMap.empty mtxvertexl;;
	
let graphhelper g tpl = StringDigraph.add_vertex g (snd tpl);;
	
List.iter (graphhelper locksetg) mtxvertexl;;


(* str2vertexmap = StringMap.add str (StringDigraph.V.create str) str2vertexmap;; *)

printf "vertex size %d\n" (StringDigraph.nb_vertex locksetg);; 

(*
let rec create_vertex lst = 
	match lst with
	GVar(varinfo,_,_)::xlst -> ((Digraph.add_vertex lockgraph 1); create_vertex xlst;)
	| _ -> [];;

 let vertex_list = create_vertex mutexl;; *)
 
let emitDep acquire hold = 
	StringDigraph.add_edge locksetg (StringMap.find hold str2vertexmap) (StringMap.find acquire str2vertexmap);;

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
											StringSet.iter (emitDep arginfo.vname) lockset;
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

(* processStmt (hd funl).sallstmts;;*)

(* List.iter (processStmt) funl;; *)

let rec processFuns fl = 
	match fl with
		f::xfl -> ( processStmt f.sallstmts; processFuns xfl;)
		| _-> ();;
		
processFuns funl;;

exception Found of StringDigraph.V.t
let find_string_vertex g i =
    try
      StringDigraph.iter_vertex (fun v -> if StringDigraph.V.label v = i then raise (Found v)) g;
      raise Not_found
    with Found v ->
      v;;

let g' = OP.transitive_closure locksetg;;
let s = StringDigraph.V.label (StringMap.find "mtx1" str2vertexmap);;


let rec search g src tgt cur k path res= 
	if(k>0 && tgt=cur) then (VertexSetSet.add path res)
	else if (k<=0) then (VertexSetSet.empty)
	else (
	let succlst = StringDigraph.succ g cur in
	List.fold_left (fun res sc -> 
		let path = VertexSet.add sc path in
		search g src tgt sc (k-1) path res) res succlst
	);;
	


StringDigraph.iter_edges (fun v0 v1 -> 
	let v0' = find_string_vertex g' (StringDigraph.V.label v0) in 
	let v1' = find_string_vertex g' (StringDigraph.V.label v1) in
	if StringDigraph.mem_edge g' v1' v0' then  (** cycle between v0 and v1 exists in original graph *)
	let accu = VertexSet.empty in
	let res  = VertexSetSet.empty in
	let newres = search locksetg v0 v1 v0 3 accu res in
	(* printf "----path size %d\n" (List.length (VertexSetSet.elements newres)); *)
	VertexSetSet.iter (fun b -> ( VertexSet.iter (fun a-> let a = StringDigraph.V.label a in printf "%s " a))  b) newres
	
	) locksetg;;	
	
	
	
(*let rec search g src tgt cur k path res= 
	if(k>0 && tgt=cur) then (StringSetSet.add path res)
	else if (k<=0) then (StringSetSet.empty)
	else (
	let succlst = StringDigraph.succ g cur in
	List.fold_left (fun res sc -> 
		let path = StringSet.add (StringDigraph.V.label sc) path in
		search g src tgt sc (k-1) path res) res succlst
	);;
	
StringDigraph.iter_edges (fun v0 v1 -> 
	let v0' = find_string_vertex g' (StringDigraph.V.label v0) in 
	let v1' = find_string_vertex g' (StringDigraph.V.label v1) in
	if StringDigraph.mem_edge g' v1' v0' then  (** cycle between v0 and v1 exists in original graph *)
	let accu = StringSet.empty in
	let res  = StringSetSet.empty in
	let newres = search locksetg v0 v1 v0 3 accu res in
	(* printf "----path size %d\n" (List.length (VertexSetSet.elements newres)); *)
	StringSetSet.iter (fun a-> let a = StringDigraph.V.label a in printf "") newres
	
	) locksetg;;	*)
	
(*
let print_main_cfg = function
	GFun(fundec , location) -> if(fundec.svar.vname="main") then printCfgFilename "dumbout" fundec
	|_ -> ();;

List.iter (print_main_cfg) f.globals;;*)

printf "graph edge size %d\n" (StringDigraph.nb_edges locksetg);; 
printf "transitive closure edge size %d\n" (StringDigraph.nb_edges g');; 
