TARGET="example/deadlock.c"
PATH=$PATH:/home/henry/cil-1.6.0/bin
MYINCLUDE="./include/"
MAINFILE=main
export PATH
cilly --save-temps -D HAPPY_MOOD $TARGET -lpthread
#ocamlc -I $MYINCLUDE -I ./include/ocamlgraph/src -I ./include/ocamlgraph/lib -ccopt -L$MYINCLUDE -ccopt -L./include/ocamlgraph/src -ccopt -L./include/ocamlgraph/lib -c $MAINFILE.ml
ocamlc -I $MYINCLUDE -I ../ocamlgraph/src -I ../ocamlgraph/lib -ccopt -L$MYINCLUDE -ccopt -L../ocamlgraph/src -ccopt -L../ocamlgraph/lib -c $MAINFILE.ml
ocamlc -o $MAINFILE unix.cma str.cma nums.cma $MYINCLUDE/cil.cma ../ocamlgraph/graph.cma $MAINFILE.cmo

