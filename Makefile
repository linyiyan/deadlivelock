CC=ocamlc

CIL_INCLUDE=../cil-1.6.0/obj/x86_LINUX
GRAPH=../ocamlgraph
GRAPH_SRC=../ocamlgraph/src
GRAPH_LIB=../ocamlgraph/lib
INCLUDE_VPATH=$(CIL_INCLUDE) $(GRAPH_SRC) $(GRAPH_LIB) $(GRAPH)
CCOPT_VPATH=$(CIL_INCLUDE) $(GRAPH_SRC) $(GRAPH_LIB) $(GRAPH)
INCLUDE=$(addprefix -I , $(INCLUDE_VPATH))
CCOPT=$(addprefix -ccopt -L, $(CCOPT_VPATH))
EXE=main
LIB=unix.cma str.cma nums.cma $(CIL_INCLUDE)/cil.cma $(GRAPH)/graph.cma 
OBJ=dlgraph.cmo dlthread.cmo lockset.cmo main.cmo
FLAG= -c

all: main

main: $(OBJ)
	ocamlc -o $(EXE) $(LIB) $(OBJ)

dlgraph.cmo : dlgraph.ml
	$(CC) $(INCLUDE) $(CCOPT) $(FLAG) dlgraph.ml

dlthread.cmo : dlthread.ml
	$(CC) $(INCLUDE) $(CCOPT) $(FLAG) dlthread.ml

lockset.cmo : lockset.ml
	$(CC) $(INCLUDE) $(CCOPT) $(FLAG) lockset.ml
	
main.cmo : main.ml
	$(CC) $(INCLUDE) $(CCOPT) $(FLAG) main.ml