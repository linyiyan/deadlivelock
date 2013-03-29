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

let print_main_cfg = function
	GFun(fundec , location) -> if(fundec.svar.vname="main") then printCfgFilename "dumbout" fundec
	|_ -> ();;

List.iter (print_main_cfg) f.globals;;
