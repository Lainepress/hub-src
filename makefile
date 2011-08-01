
all: hub


hub: hub.hs
	ghc --make -o hub hub.hs

install: hub
	cp hub /usr/local/bin/hub

clean:
	rm -f *.hi *.o *~

distclean: clean
	rm hub
