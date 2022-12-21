
build:
	@echo "\n==== COMPILING FORD FULKERSON ====\n"
	ocamlbuild ftest.native
	@echo "\n==== COMPILING MAXIMUM BIPARTITE MATCHING ====\n"
	ocamlbuild mbptest.native

format:
	ocp-indent --inplace src/*

edit:
	code . -n

demo: build
	@echo "\n==== EXECUTING FORD FULKERSON ====\n"
	./ftest.native graphs/graph1 0 5 outfile_ff
	@echo "\n==== RESULT ==== (content of outfile_ff) \n"
	@cat outfile_ff

	@echo "\n==== EXECUTING MAXIMUM BIPARTITE MATCHING ====\n"
	./mbptest.native graphs/mbp_graph outfile_mbp
	@echo "\n==== RESULT ==== (content of outfile_mbp) \n"
	@cat outfile_mbp

clean:
	-rm -rf _build/
	-rm ftest.native
	-rm mbptest.native
