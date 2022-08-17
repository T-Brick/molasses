SOURCES =$(shell find src -iname "*.sml" -o -iname "*.mlb")

molasses: $(SOURCES)
	mlton -mlb-path-var 'COMPAT mlton' -output molasses src/molasses.mlb

bootstrap-lib:
	./molasses --repl -mlb-path-var 'COMPAT nj' -output bootstrap-test parse-sml/src/base/lib/sources.mlb

bootstrap-ast:
	./molasses --repl -mlb-path-var 'COMPAT nj' -output bootstrap-test parse-sml/src/ast/sources.mlb

bootstrap-parse:
	./molasses --repl -mlb-path-var 'COMPAT nj' -output bootstrap-test parse-sml/src/parse/sources.mlb

bootstrap:
	./molasses --repl -mlb-path-var 'COMPAT nj' -output bootstrap-test src/molasses.mlb

.PHONY: clean
clean:
	rm -f molasses && find . -type d -name '.molasses' -exec rm -r {} + && rm -rf bootstrap-test
