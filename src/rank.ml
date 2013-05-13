open Map
open Dlutil
open Printf
open Scanf
open Yices

module Sm = Map.Make(String)

(* external z3_get_rank : string -> unit = "z3_get_rank" *)
(*
let ex1 =
  let ctx = mk_context () in
  let a = mk_bool_var ctx "a"
  and b = mk_bool_var ctx "b" 
  and c = mk_bool_var ctx "c" in
  let aandb = mk_and ctx [| a ; b |]
  and cimpliesnotb = mk_or ctx [| mk_not ctx c ; mk_not ctx b |] in
  let _ = assert_retractable ctx aandb
  and _ = assert_retractable ctx cimpliesnotb
  in begin
  match check ctx with
  | True ->
    print_endline "true";
    let m = get_model ctx
    and print_value m v = 
      match get_value m v with
      | True -> print_endline ((get_var_decl_name v)^": true")
      | False -> print_endline ((get_var_decl_name v)^": false")
      | Undef -> print_endline ((get_var_decl_name v)^": undef")
    in iter_bool_var_decl (print_value m) ctx
  | False -> print_endline "false"
  | Undef -> print_endline "undef"
  end; del_context ctx;;*)
(*
let read_all_lines file_name =
  let in_channel = open_in file_name in
  let rec read_recursive lines =
    try
      Scanf.fscanf in_channel "%[^\r\n]\n" (fun x -> read_recursive (x :: lines))
    with
      End_of_file ->
        lines in
  let lines = read_recursive [] in
  let _ = close_in_noerr in_channel in
  List.rev (lines)

let gen_solver_file (max_rank : int) (lock_var_lst : string list) (trylock_var_lst : string list) (var_diff_tuple_lst : (string*string*string) list) = 
	let oc = open_out "rank_sat.y" in
	let () = List.iter (fun v-> fprintf oc "(declare-const %s Int)\n" v) (lock_var_lst@trylock_var_lst) in 
	let () = List.iter (fun v-> fprintf oc "(assert (< %s %d))\n" v max_rank) (lock_var_lst@trylock_var_lst) in 
	let () = List.iter
	begin
		fun lock_var -> 
		List.iter
		begin
			fun trylock_var -> 
				fprintf oc "(assert (> %s %s))\n" trylock_var lock_var
		end trylock_var_lst
	end lock_var_lst
	in 
	let () = List.iter (fun (v,v',rel)->fprintf oc "(assert (%s %s %s))\n" rel v v') var_diff_tuple_lst
	in 
	let () = fprintf oc "(check-sat)\n" in
	let () = close_out oc in ()

let parse_file : int Sm.t = 
	let file_lines = read_all_lines "tempresult" in
	List.fold_left
	begin
		fun rm line -> 
			let f = fun v r -> (v,r) in
			let (v,r) = sscanf line "%s -> %d" f in 
			Sm.add v r rm
	end Sm.empty file_lines


	
	
let get_var_rank (max_rank : int) (lock_var_lst : string list) (trylock_var_lst : string list) (var_diff_tuple_lst : (string*string*string) list) : int Sm.t= 
	let () = gen_solver_file (max_rank) (lock_var_lst) (trylock_var_lst) (var_diff_tuple_lst) in
	let _ = Sys.command "./z3_rank_solver >> tempresult" in 
	let res = parse_file in res
	(*let rbindings = Sm.bindings res in
	let () = List.iter (fun b -> printf "%s : %d\n" (fst b) (snd b)) rbindings in
	(*let _ = Sys.command "rm tempresult" in ()*)
	()*)
*)

(*
let get_var_rank (max_rank : int) (lock_var_lst : string list) (trylock_var_lst : string list) (var_diff_tuple_lst : (string*string*string) list) : int Sm.t=  
	let ctx = mk_context () in
	let itype = mk_type ctx "int" in
	let xdecl = mk_var_decl ctx "x" itype in
	let ydecl = mk_var_decl ctx "y" itype in
	let x = mk_var_from_decl ctx xdecl in
	let y = mk_var_from_decl ctx ydecl in
	let n1 = mk_num ctx 1 in
	let n2 = mk_num ctx 3 in
	let eq1 = mk_eq ctx x n1 in
	let eq2 = mk_eq ctx y n2 in
	let aandb = mk_and ctx [| eq1 ; eq2 |] in
	let _ = assert_retractable ctx aandb in
	let _ = 
		match check ctx with
		| True ->
		begin
			let m = get_model ctx in
			let yval = get_int_value m ydecl in 
			printf "%d \n" (Int32.to_int yval);
		end
		| False -> print_endline "false"
		| Undef -> print_endline "undef"
	in
	Sm.empty *)

let get_var_rank (max_rank : int) (lock_var_lst : string list) (trylock_var_lst : string list) (var_diff_tuple_lst : (string*string*string) list) : int Sm.t=  
	let ctx = mk_context () in
	let itype = mk_type ctx "int" in
	let max_rank_num = mk_num ctx max_rank in
	let lock_var_list = lock_var_lst@trylock_var_lst in
	let decl_map = List.fold_left (fun dl_map var -> let dl = mk_var_decl ctx var itype in Sm.add var dl dl_map) Sm.empty lock_var_list in
	let yvar_map = List.fold_left (fun yv_map var -> let dl = Sm.find var decl_map in let yv = mk_var_from_decl ctx dl in Sm.add var yv yv_map) Sm.empty lock_var_list in
	let lessthan_max_array = Array.init (List.length lock_var_list) (fun i->let vname = List.nth lock_var_list i in let yvar = Sm.find vname yvar_map in mk_lt ctx yvar max_rank_num) in (* every variable less than max rank *)
	let lessthan_max = mk_and ctx lessthan_max_array in
	let try_gt_lock_list = 
	List.fold_left
	begin
		fun rl lvar->
		let tmp = 
		List.fold_left
		begin
			fun innerrl tlvar -> 
				let lvar_y = Sm.find lvar yvar_map in
				let tlvar_y = Sm.find tlvar yvar_map in
				let less_exp = mk_lt ctx lvar_y tlvar_y in
				less_exp::innerrl
		end
		[] trylock_var_lst
		in tmp@rl
	end [] lock_var_lst
	in
	let try_gt_lock_array = Array.init(List.length try_gt_lock_list) (fun i->List.nth try_gt_lock_list i) in
	let try_gt_lock = if (Array.length try_gt_lock_array)>0 then mk_and ctx try_gt_lock_array else mk_true ctx in
	let lock_compare_list = 
	List.fold_left
	begin
		fun rl (v0,v1,rel) -> 
			let v0_y = Sm.find v0 yvar_map in
			let v1_y = Sm.find v1 yvar_map in
			let temp = 
				if rel = "<" then mk_lt ctx v0_y v1_y
				else if rel = "=" then mk_eq ctx v0_y v1_y
				else mk_lt ctx v1_y v0_y 
			in
			temp :: rl
	end [] var_diff_tuple_lst
	in
	let lock_compare_array = Array.init(List.length lock_compare_list) (fun i->List.nth lock_compare_list i) in
	let lock_compare = if(Array.length lock_compare_array)>0 then mk_and ctx lock_compare_array else mk_true ctx in
	let rank_formula = mk_and ctx [| lessthan_max ; try_gt_lock ; lock_compare |] in
	let _ = assert_retractable ctx rank_formula in
	let res = 
		match check ctx with
		| True ->
		begin
			let m = get_model ctx in
			List.fold_left
			begin
				fun rm lk -> 
					let v_val32 = get_int_value m (Sm.find lk decl_map) in
					let v_val = Int32.to_int v_val32 in
					Sm.add lk v_val rm
			end Sm.empty lock_var_list
			(* let yval = get_int_value m ydecl in 
			printf "%d \n" (Int32.to_int yval); *)
			
		end
		
		| False -> Sm.empty
		| Undef -> Sm.empty
	in 
	res

let rank (stmt2satvarname : string Sm.t) (diff_tuple_lst : (string*string*string) list) : int Sm.t = 
	let var_num = Sm.cardinal stmt2satvarname in
	let max_rank = var_num + 1 in
	let (lock_var_lst,trylock_var_lst) = 
	Sm.fold 
	(fun stmt var (lklst,tlklst) -> let (_,_,opr,_)=parse_stmt_str_info stmt in if opr="pthread_mutex_lock" then (var::lklst,tlklst) else (lklst,var::tlklst)) 
	(stmt2satvarname) ([],[]) in
	let var_diff_tuple_lst = List.fold_left (fun rl (s1,s2,rel) -> (Sm.find s1 stmt2satvarname, Sm.find s2 stmt2satvarname, rel)::rl) [] diff_tuple_lst in
	get_var_rank (max_rank) (lock_var_lst) (trylock_var_lst) (var_diff_tuple_lst)
		