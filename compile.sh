TARGET="example/deadlock.c"
PATH=$PATH:/home/henry/cil-1.6.0/bin
MYINCLUDE="./include/"
MAINFILE=main
export PATH
cilly --save-temps -D HAPPY_MOOD $TARGET -lpthread
#ocamlc -I /home/henry/cil-1.6.0/obj/x86_LINUX/ -ccopt -L/home/henry/cil-1.6.0/obj/x86_LINUX/ -c main.ml
ocamlc -I $MYINCLUDE -I ./include/ocamlgraph/src -I ./include/ocamlgraph/lib -ccopt -L$MYINCLUDE -ccopt -L./include/ocamlgraph/src -ccopt -L./include/ocamlgraph/lib -c $MAINFILE.ml
ocamlc -o $MAINFILE unix.cma str.cma nums.cma $MYINCLUDE/cil.cma $MYINCLUDE/graph.cma $MAINFILE.cmo

