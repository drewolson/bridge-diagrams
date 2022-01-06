list-files = git ls-files '*.hs'

ormolu = stack exec -- ormolu -o '-XImportQualifiedPost' -o '-XPatternSynonyms'

build:
	stack build

format: build
	$(ormolu) --mode inplace $(shell $(list-files))

format-check: build
	$(ormolu) --mode check $(shell $(list-files))

.PHONY: build format format-check
