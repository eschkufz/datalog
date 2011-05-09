all:
	ghc --make -package parsec datalog.hs

clean:
	rm -f datalog datalog.hi datalog.o
