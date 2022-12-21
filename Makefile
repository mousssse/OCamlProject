
build:
	@echo "\n==== COMPILING FORD FULKERSON ====\n"
	ocamlbuild ftest.native
	@echo "\n==== COMPILING MAXIMUM BIPARTITE MATCHING ====\n"
	ocamlbuild mbptest.native
	@echo "\n==== COMPILING BUSACKER GOWEN ====\n"
	ocamlbuild bgtest.native

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

	@echo "\n==== EXECUTING BUSACKER GOWEN ====\n"
	./bgtest.native graphs/bg_graph 0 4 outfile_bg
	@echo "\n==== RESULT ==== (content of outfile_bg) \n"
	@cat outfile_bg

clean:
	-rm -rf _build/
	-rm ftest.native
	-rm mbptest.native
	-rm bgtest.native