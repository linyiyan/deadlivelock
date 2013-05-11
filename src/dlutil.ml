open Printf
open Scanf
module Sm = Map.Make(String)
module Ss = Set.Make(String)

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