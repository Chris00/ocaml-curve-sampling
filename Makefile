PKGVERSION = $(shell git describe --always --dirty)

build:
	dune build @install

test:
	dune runtest --force

install uninstall:
	dune $@

doc:
	sed -e 's/%%VERSION%%/$(PKGVERSION)/' src/curve_sampling.mli \
	  > _build/default/src/curve_sampling.mli
	dune build @doc
	echo '.def { background: #f9f9de; }' >> _build/default/_doc/odoc.css

lint:
	opam lint curve-sampling.opam

clean:
	dune clean

.PHONY: build install uninstall doc lint clean
