SOURCES =$(shell find src -iname "*.sml" -o -iname "*.mlb")

cm: $(SOURCES)
	mlton -mlb-path-var 'COMPAT mlton' -output molasses src/molasses.mlb

.PHONY: clean
clean:
	rm -f cm
