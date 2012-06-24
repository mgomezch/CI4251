exec := Unisex
ghc  := ghc

.PHONY: all clean $(exec)

all: $(exec)

clean:
	rm -f ./*.hi ./*.o $(exec)

$(exec):
	$(ghc) --make $@
