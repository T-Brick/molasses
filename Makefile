SOURCES =$(shell find src -iname "*.sml" -o -iname "*.mlb")

molasses: $(SOURCES)
	mlton -mlb-path-var 'COMPAT mlton' -output molasses src/molasses.mlb

all: molasses smlnj

smlnj:
	./molasses -mlb-path-var 'COMPAT nj' -output smlnj src/molasses.mlb

.PHONY: clean
clean:
	rm -f molasses && find . -type d -name '.molasses' -exec rm -r {} + && rm -rf smlnj
