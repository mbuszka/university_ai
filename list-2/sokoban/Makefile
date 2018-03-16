FLAGS=-O2

bfs: grid.ml bfs.ml
	ocamlopt -o bfs grid.ml bfs.ml $(FLAGS)

%.cmx : %.ml
	ocamlopt -c $< $(FLAGS)

astar: grid.ml heap.ml astar.ml
	ocamlopt -o astar grid.ml heap.ml astar.ml $(FLAGS)

crates: grid.ml heap.ml crates.ml
	ocamlopt -o crates grid.ml heap.ml crates.ml $(FLAGS)

clean:
	rm -f zad_* *.cm* *.o

distclean: clean
	rm -f bfs astar crates
