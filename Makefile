
all:
	dune build

test:
	dune runtest --display quiet --no-buffer

bench:
	dune exec ./bench.exe

doc:
	dune build @doc

clean:
	dune clean
