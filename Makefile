SOURCES =$(shell find src -iname "*.sml" -o -iname "*.mlb")

molasses: $(SOURCES)
	mlton -mlb-path-var 'COMPAT mlton' -output molasses src/molasses.mlb

all: molasses smlnj

.PHONY: clean smlnj
smlnj:
	./molasses -mlb-path-var 'COMPAT nj' -output smlnj src/molasses.mlb

repl:
	make smlnj && sml src/top-nj.cm src/repl/environment.sml src/repl/make.sml

clean:
	rm -f molasses && find . -type d -name '.molasses' -exec rm -r {} + && rm -rf smlnj && find . -type f -name 'molasses-repl.*' -exec rm -r {} +
