
all:
	dune build

test:
	dune runtest -f --display quiet --no-buffer

bench:
	dune exec ./bench.exe

doc:
	dune build @doc

clean:
	dune clean
