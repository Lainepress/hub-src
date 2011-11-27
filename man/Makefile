doc: hub.1

%.1: %.xml
	xmlto -m manpage-normal.xsl  man $^

%.xml: %.txt
	asciidoc -b docbook -d manpage $^
