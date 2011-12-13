all: hub.1.gz hub.5.gz

%.gz: %
	gzip -c $< >$@

%.html: %.xml
	xmlto -m manpage-normal.xsl  html $^
	mv index.html $@

%.1: %-1.xml
	xmlto -m manpage-normal.xsl  man  $^

%.5: %-5.xml
	xmlto -m manpage-normal.xsl  man  $^

%.xml: %.txt
	asciidoc -b docbook -d manpage $^
