OD = dist/build/hub
HC = mkdir -p $(OD); ghc -cpp --make -outputdir build -Wall

all: hub

hub: Hub/Help.hs
	$(HC) -cpp -Wall --make -o $(OD)/hub hub.hs

Hub/Help.hs: help.txt
	runhaskell prep

install:
	cp $(OD)/hub /usr/hs/bin

clean:
	cabal clean
	rm -rf build
