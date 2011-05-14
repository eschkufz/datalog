all:
	ghc -Wall -O2 --make -package parsec Datalog.hs
	ghc -Wall -O2 --make Main.hs

clean:
	rm -f Main *.hi *.o
