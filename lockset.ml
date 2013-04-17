open Printf
open Set
open Dlgraph
open Dlthread


let marklockset_tss tss tfm = Tss.iter (fun elt -> printf "lockset\n" ) tss