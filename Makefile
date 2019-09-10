all: build mandelbrot

build:
	@dune build

mandelbrot:
	@ln -s _build/install/default/bin/$@ ./$@

install:
	@dune install

clean:
	@dune clean

uninstall:
	@dune install
