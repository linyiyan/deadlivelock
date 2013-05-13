CC=ocamlc

CIL_INCLUDE=../cil-1.6.0/obj/x86_LINUX
SRC_DIR=./src
GRAPH=../ocamlgraph
GRAPH_SRC=../ocamlgraph/src
GRAPH_LIB=../ocamlgraph/lib
Z3_LIB=../z3ocaml/z3/ocaml
YICES=../ocamlyices
INCLUDE_VPATH=$(CIL_INCLUDE) $(GRAPH_SRC) $(GRAPH_LIB) $(GRAPH) $(YICES) $(SRC_DIR) 
CCOPT_VPATH=$(CIL_INCLUDE) $(GRAPH_SRC) $(GRAPH_LIB) $(GRAPH)
INCLUDE=$(addprefix -I , $(INCLUDE_VPATH))
CCOPT=$(addprefix -ccopt -L, $(CCOPT_VPATH))
EXE=main
LIBS=unix.cma str.cma nums.cma $(YICES)/ocamlyices.cma $(CIL_INCLUDE)/cil.cma $(GRAPH)/graph.cma 
OBJS= dlutil.cmo heuristic.cmo dlgraph.cmo dlthread.cmo lockset.cmo deadlivelock.cmo rank.cmo maxsat_file.cmo yicesgen.cmo main.cmo
COBJS = 
FLAG= -c

all: main

main: $(OBJS) $(COBJS)
	ocamlc -o $(EXE)  $(LIBS) $(COBJS) $(addprefix $(SRC_DIR)/, $(OBJS))
	
dlutil.cmo : $(SRC_DIR)/dlutil.ml
	$(CC) $(INCLUDE) $(CCOPT) $(FLAG) $(SRC_DIR)/dlutil.ml
	
heuristic.cmo : $(SRC_DIR)/heuristic.ml
	$(CC) $(INCLUDE) $(CCOPT) $(FLAG) $(SRC_DIR)/heuristic.ml

dlgraph.cmo : $(SRC_DIR)/dlgraph.ml
	$(CC) $(INCLUDE) $(CCOPT) $(FLAG) $(SRC_DIR)/dlgraph.ml

dlthread.cmo : $(SRC_DIR)/dlthread.ml
	$(CC) $(INCLUDE) $(CCOPT) $(FLAG) $(SRC_DIR)/dlthread.ml

lockset.cmo : $(SRC_DIR)/lockset.ml
	$(CC) $(INCLUDE) $(CCOPT) $(FLAG) $(SRC_DIR)/lockset.ml
	
deadlivelock.cmo : $(SRC_DIR)/deadlivelock.ml
	$(CC) $(INCLUDE) $(CCOPT) $(FLAG) $(SRC_DIR)/deadlivelock.ml
	
z3_rank_solver.o : $(SRC_DIR)/z3_rank_solver.c
	gcc -fopenmp -c $(SRC_DIR)/z3_rank_solver.c -I ../../z3ocaml/z3/include -L ../../z3ocaml/z3/lib -lz3

z3ocaml.o : $(SRC_DIR)/z3ocaml.c
	$(CC) -I ./ -c $(SRC_DIR)/z3ocaml.c
	
rank.cmo : $(SRC_DIR)/rank.ml
	$(CC) $(INCLUDE) $(CCOPT) $(FLAG) $(SRC_DIR)/rank.ml
	
maxsat_file.cmo : $(SRC_DIR)/maxsat_file.ml
	$(CC) $(INCLUDE) $(CCOPT) $(FLAG) $(SRC_DIR)/maxsat_file.ml
	
yicesgen.cmo : $(SRC_DIR)/yicesgen.ml
	$(CC) $(INCLUDE) $(CCOPT) $(FLAG) $(SRC_DIR)/yicesgen.ml
	
main.cmo : $(SRC_DIR)/main.ml
	$(CC) $(INCLUDE) $(CCOPT) $(FLAG) $(SRC_DIR)/main.ml
	
.PHONY : example test1 test_ifthenelse1 test_ifthenelse2 test_while1 test_trylock_unlock_missing test_trylock_while_livelock z3_rank_solver

example : 
	cilly --save-temps -D HAPPY_MOOD example/simple/deadlock.c -lpthread

test1 : 
	cilly --save-temps -D HAPPY_MOOD example/simple/test1.c -lpthread
	
test_ifthenelse1 : 
	cilly --save-temps -D HAPPY_MOOD example/simple/test_ifthenelse1.c -lpthread

test_ifthenelse2 : 
	cilly --save-temps -D HAPPY_MOOD example/simple/test_ifthenelse2.c -lpthread
	
test_while1 : 
	cilly --save-temps -D HAPPY_MOOD example/simple/test_while1.c -lpthread

test_trylock_unlock_missing:
	cilly --save-temps -D HAPPY_MOOD example/simple/test_trylock_unlock_missing.c -lpthread

test_trylock_while_livelock:
	cilly --save-temps -D HAPPY_MOOD example/simple/test_trylock_while_livelock.c -lpthread
	
z3_rank_solver: 
	gcc -fopenmp -o z3_rank_solver $(SRC_DIR)/z3_rank_solver.c -I ../z3ocaml/z3/include -L ../z3ocaml/z3/lib -lz3