OD = dist/build/hub
ID = /usr/hs/bin
HC = mkdir -p $(OD); ghc -cpp --make -outputdir build -Wall

all: hub

hub: Hub/Help.hs
	$(HC) -cpp -Wall --make -o $(OD)/hub hub.hs

Hub/Help.hs: help.txt
	runhaskell prep

install:
	mkdir -p $(ID)
	cp $(OD)/hub $(ID)

clean:
	cabal clean
	rm -rf build
