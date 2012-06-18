SHELL = bash


.PHONY: all clean rtree

all: rtree

clean:
	shopt -s globstar && rm -f rtree ./**/*.hi{,-boot} ./**/*.o{,-boot}

rtree:
	ghc --make -Wall rtree
