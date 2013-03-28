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

let l=f.globals in
let mutexl = List.filter (isMutexGvar) l in
printf "%d\n" (List.length mutexl);;
