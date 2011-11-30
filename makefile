OD = dist/build/hub
ID = /usr/hs/bin
HC = mkdir -p $(OD); ghc -cpp --make -outputdir build -Wall

all: hub

hub: prep
	$(HC) -cpp -Wall --make -o $(OD)/hub hub.hs

prep:
	runhaskell prep

install:
	mkdir -p $(DESTDIR)$(ID)
	cp $(OD)/hub $(DESTDIR)$(ID)

clean:
	cabal clean
	rm -rf build
