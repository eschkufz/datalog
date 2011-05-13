all:
	ghc -Wall --make -package parsec datalog.hs

clean:
	rm -f datalog datalog.hi datalog.o
