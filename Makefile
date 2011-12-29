OD = dist/build/hub
ID = /usr/hs/bin
HC = mkdir -p $(OD); ghc -cpp -XHaskell2010 --make -outputdir build -Wall

all: hub

hub: prep
	$(HC) -cpp -Wall --make -o $(OD)/hub hub.hs

prep:
	runhaskell prep

install:
	install -D $(OD)/hub    $(DESTDIR)$(ID)/hub
	install -D man/hub.1.gz $(DESTDIR)/usr/share/man/man1/hub.1.gz
	install -D man/hub.5.gz $(DESTDIR)/usr/share/man/man5/hub.5.gz

clean:
	cabal clean
	rm -rf build
