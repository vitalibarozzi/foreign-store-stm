object/Game.o:
	echo -e ":set -fobject-code\n:r\n:q" | stack ghci
	mv .stack-work/odir/Game.o object/Game.o

.PHONY: build
build: object/Game.o

.PHONY: clean
clean:
	stack clean
	rm -r object/*
