CC=ocamlc

CIL_INCLUDE=../cil-1.6.0/obj/x86_LINUX
GRAPH=../ocamlgraph
GRAPH_SRC=../ocamlgraph/src
GRAPH_LIB=../ocamlgraph/lib
INCLUDE_VPATH=$(CIL_INCLUDE) $(GRAPH_SRC) $(GRAPH_LIB) $(GRAPH)
CCOPT_VPATH=$(CIL_INCLUDE) $(GRAPH_SRC) $(GRAPH_LIB) $(GRAPH)
INCLUDE=$(addprefix -I , $(INCLUDE_VPATH))
CCOPT=$(addprefix -ccopt -L, $(CCOPT_VPATH))
SRC=./src
EXE=main
LIB=unix.cma str.cma nums.cma $(CIL_INCLUDE)/cil.cma $(GRAPH)/graph.cma 
OBJ=dlgraph.cmo dlthread.cmo lockset.cmo main.cmo
FLAG= -c

all: main

main: $(OBJ)
	ocamlc -o $(EXE) $(LIB) $(OBJ)

dlgraph.cmo : $(SRC)/dlgraph.ml
	$(CC) $(INCLUDE) $(CCOPT) $(FLAG) $(SRC)/dlgraph.ml

dlthread.cmo : $(SRC)/dlthread.ml
	$(CC) $(INCLUDE) $(CCOPT) $(FLAG) $(SRC)/dlthread.ml

lockset.cmo : $(SRC)/lockset.ml
	$(CC) $(INCLUDE) $(CCOPT) $(FLAG) $(SRC)/lockset.ml
	
main.cmo : $(SRC)/main.ml
	$(CC) $(INCLUDE) $(CCOPT) $(FLAG) $(SRC)/main.ml
	
.PHONY : example test1
example : 
	PATH=$PATH:/home/henry/cil-1.6.0/bin | export PATH | cilly --save-temps -D HAPPY_MOOD example/simple/deadlock.c -lpthread

test1 : 
	PATH=$PATH:/home/henry/cil-1.6.0/bin | export PATH | cilly --save-temps -D HAPPY_MOOD example/simple/test1.c -lpthread