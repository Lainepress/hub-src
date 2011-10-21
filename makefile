OD = dist/build/hub
ID = /usr/hs/bin
HC = mkdir -p $(OD); ghc -cpp --make -outputdir build -Wall

all: hub

hub: prep
	$(HC) -cpp -Wall --make -o $(OD)/hub hub.hs

prep:
	runhaskell prep

install:
	mkdir -p $(ID)
	cp $(OD)/hub $(ID)

clean:
	cabal clean
	rm -rf build
