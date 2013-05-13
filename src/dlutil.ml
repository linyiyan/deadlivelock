open Printf
open Scanf
module Sm = Map.Make(String)
module Ss = Set.Make(String)

let vertex_lst_to_str_lst g vlst = 
	List.fold_left
	begin
		fun strlst v -> 		
		 (g#label v)::strlst
	end
	[] vlst


let print_str_lst lst = List.iter (printf "%s ") lst

let print_str_lsts lsts = List.iter (fun lst -> print_str_lst lst; printf "\n") lsts


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

(*get previous element in the list *)
let rec get_prev_elt (elt:string) (lst : string list) : string= 
	match lst with
	| e1::e2::xlst -> if elt=e2 then e1 else get_prev_elt elt (e2::xlst)
	| _ -> ""
	

let print_diff_tuple_list tpl_lst = 
	List.iter
	begin
		fun tpl -> 
		match tpl with
		| (s1,s2,rel) -> printf "%s %s %s\n" s1 rel s2
		| _ -> ()
	end
	tpl_lst


(* return the last element in the list *)
let rec last_elem lst = 
	match lst with
	| x::[] -> x
	| x::xlst -> last_elem xlst
	| _ -> raise Not_found

(* return unique combinations of a string list (2 elements in each combination)*)
let rec unique_combinations2 (lst : string list) : (string*string) list = 
	match lst with
	| x::xlst -> 
		let temp_comb = 
			List.fold_left
			begin
				fun r y -> (x,y)::r
			end [] xlst
		in
		temp_comb @ unique_combinations2 (xlst)
	| x::[] -> []
	| _-> []

(*string tokenizer*)
let rec split_char sep str =
  try
    let i = String.index str sep in
    String.sub str 0 i ::
      split_char sep (String.sub str (i+1) (String.length str - i - 1))
  with Not_found ->
    [str]
	

(* decompose lock statement -> (threadfunc,lock set, lock operation, lock) *)
let parse_stmt_str_info (stmt:string) : (string*(string list)*string*string) = 
	let substrs = split_char ';' stmt in
	match substrs with 
	|threadFunc :: hold_locks :: opr :: acq_lock :: [] -> 
		let hold_locks_lst = split_char ',' hold_locks in
		(threadFunc,hold_locks_lst,opr,acq_lock)
	|_->("",[""],"","")
	
(* find lock statement given a thread func and a lock, in stmt2lockset mapping*)
let find_lock_str (threadFunc: string) (acq_lock : string) (stmtmap : Ss.t Sm.t) : string = 
	let bindings = Sm.bindings stmtmap in
	let rec rfind_lock_str (threadFunc: string) (acq_lock : string) (bindings : (Sm.key*Ss.t) list) : string = 
		match bindings with
		| (k,_)::xbindings -> let (tf,_,_,al) = parse_stmt_str_info k in 
			if tf=threadFunc && al=acq_lock then k
			else rfind_lock_str (threadFunc) (acq_lock) (xbindings)
		| _ -> ""
	in rfind_lock_str threadFunc acq_lock bindings
	

(* generate variable names for max sat solver *)
let gen_stmt2satvarname (deplist : string list list) : string Sm.t = 
	let rec rgen (slst:string list) (i:int) (res : string Sm.t): string Sm.t = 
		begin
			match slst with
			| x::xslst -> let varname = sprintf "v%d" i in 
							let res = Sm.add x varname res
							in rgen xslst (i+1) res 
			| [] -> res
		end
	in 
	let stmtset = List.fold_left
	begin
		fun sres dep -> 
			let sr = 
			List.fold_left 
			begin
				fun sr stmt -> Ss.add stmt sr
			end sres dep
			in
			Ss.union sr sres
	end
	Ss.empty deplist
	in let res = Sm.empty 
	in rgen (Ss.elements stmtset) (0) res









