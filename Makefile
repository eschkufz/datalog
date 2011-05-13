all:
	ghc -Wall -O2 --make -package parsec datalog.hs

clean:
	rm -f datalog datalog.hi datalog.o
