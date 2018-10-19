
all: lib simple

lib:
	dune build -p repl

simple:
	dune build example/simple.exe

run: simple
	_build/default/example/simple.exe
