SOURCES =$(shell find src -iname "*.sml" -o -iname "*.mlb")

molasses: $(SOURCES)
	mlton -mlb-path-var 'COMPAT mlton' -output molasses src/molasses.mlb

.PHONY: clean
clean:
	rm -f molasses && find . -type d -name '.molasses' -exec rm -r {} +
