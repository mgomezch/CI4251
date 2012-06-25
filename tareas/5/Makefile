exec := Unisex
ghc  := ghc

.PHONY: all clean $(exec)

all: $(exec)

clean:
	rm -f ./*.aux ./*.hi ./*.log ./*.o ./*.out ./*.pdf $(exec)

$(exec):
	$(ghc) --make $@
