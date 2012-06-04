OD = dist/build/hub
ID = /usr/hs/bin
HC = mkdir -p $(OD); ghc -cpp -XHaskell2010 --make -outputdir build -Wall

all: hub

hub: prep
	$(HC) -Wall --make -O1 -o $(OD)/hub hub.hs

prep:
	hub load    build-hub <build-hub.har                  || printf '\nNo hub on path: ensure hub-src.har or hub.cabal packages are installed\n\n'
	hub comment build-hub "Hub for building the hub tool" || true
	hub set     build-hub                                 || true
	runhaskell prep

install:
	install -D $(OD)/hub    $(DESTDIR)$(ID)/hub
	install -D man/hub.1.gz $(DESTDIR)/usr/share/man/man1/hub.1.gz
	install -D man/hub.5.gz $(DESTDIR)/usr/share/man/man5/hub.5.gz

clean:
	cabal clean
	rm -rf build
