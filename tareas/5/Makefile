exec     := Unisex
ghc      := ghc
ghc-opts := -Wall


.PHONY: all clean $(exec)

all: $(exec)

clean:
	rm -f ./*.hi ./*.o $(exec)

$(exec):
	$(ghc) $(ghc-opts) --make $@
