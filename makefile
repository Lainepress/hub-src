OD = dist/build/hub
HC = mkdir -p $(OD); ghc -cpp --make -outputdir build -Wall

all: hub

hub:
	$(HC) -cpp -Wall --make -o $(OD)/hub hub.hs

install:
	cp $(OD)/hub /usr/local/bin/hub

clean:
	cabal clean
	rm -rf build
