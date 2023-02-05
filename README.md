# OcamlProject
This is an Ocaml project on Ford-Fulkerson, Maximum Bipartite Matching and Busacker Gowen algorithms, developed by [Sandro Chalhoub](https://github.com/54ndro18) and [Sarah Mousset](https://github.com/mousssse). 
This project contains some simple configuration files to facilitate editing Ocaml in VSCode.

To use, you should install the *OCaml* extension in VSCode. Other extensions might work as well but make sure there is only one installed.
Then open VSCode in the root directory of this repository (command line: `code path/to/ocaml-maxflow-project`).

Features :
 - full compilation as VSCode build task (Ctrl+Shift+b)
 - highlights of compilation errors as you type
 - code completion
 - automatic indentation on file save


## Infiles and outfiles
Outfiles can be visualised on [Graphviz Visual Editor](http://magjac.com/graphviz-visual-editor/).
To visualise infiles, you can use the `export` function from the [Gfile](src/gfile.mli) package. Infiles have specific formats:

### 1. Ford-Fulkerson infile format
```
%% Ford-fulkerson infile
%% This is a comment

%% A node is represented by the letter n, followed by its coordinates - which aren't used by the algorithms
n 28 321   % This is node #0
n 92 15    % This is node #1
n 47 37    % This is node #2

%% An arc is represented by the letter e, followed by the source and destination nodes, then the label
e 0 1 28   % An arc from #0 to #1, labelled "28"
e 2 0 6    % An arc from #2 to #0, labelled "28"
```
### 2. Maximum Bipartite Matching infile format
The following file follows the example given [here](https://www.geeksforgeeks.org/maximum-bipartite-matching/).
```
%% Maximum Bipartite Matching infile
%% This is a comment

%% From/To node names
1 applicant   % the title for the nodes that have out arcs
2 job         % The title for the nodes that have in arcs

%% To nodes (nodes that only have in arcs)
t     % This is "to node" #0
t
t
t
t
t     % This is "to node" #5.

%% From nodes (nodes that only have out arcs) + arcs
f [1 2]    %there is an arc from first node f to 1 and from f to 2
f []       %there are no arcs from second node f
f [0 3]    
f [2] 
f [2 3] 
f [5] 
```
### 3. Busacker Gowen infile format
```
%% Busacker Gowen infile
%% This is a comment

%% Nodes
n 88 209      % This is node #0, with its coordinates - which are not used by the algorithms
n 408 183
n 269 491
n 261 297
n 401 394     % This is node #4.

%% Arcs
e 0 1 30 7    % An arc from #0 to #1, of flow capacity 30 and cost 7.
e 0 2 20 6
e 1 2 25 5
e 1 3 10 4
e 2 3 20 2
e 2 4 25 2
e 3 4 20 1
```

## Build and run
A makefile provides some useful commands:
 - `make build` to compile. This creates ftest.native, mbptest.native and bgtest.native executables
 - `make demo` to run the `ftest`, `mbptest` and `bgtest` programs with some arguments
 - `make format` to indent the entire project
 - `make edit` to open the project in VSCode
 - `make clean` to remove build artifacts

In case of trouble with the VSCode extension (e.g. the project does not build, there are strange mistakes), a common workaround is to (1) close vscode, (2) `make clean`, (3) `make build` and (4) reopen vscode (`make edit`).
