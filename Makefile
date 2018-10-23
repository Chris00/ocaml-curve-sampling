PKGVERSION = $(shell git describe --always --dirty)

build:
	dune build @install

test:
	$(RM) -f $(wildcard _build/default/tests/*.pdf)
	dune runtest
	dune build @latex

install uninstall:
	dune $@

doc:
	sed -e 's/%%VERSION%%/$(PKGVERSION)/' src/curve_sampling.mli \
	  > _build/default/src/curve_sampling.mli
	dune build @doc
	@echo '.def { background: #f0f0f0; }' \
	  >> _build/default/_doc/_html/odoc.css
	@echo See _build/default/_doc/_html/index.html

lint:
	opam lint curve-sampling.opam

clean:
	dune clean

.PHONY: build install uninstall doc lint clean
