format:
	stack exec -- ormolu -o '-XImportQualifiedPost' -o '-XPatternSynonyms' --mode inplace $(shell git ls-files '*.hs')

format-check:
	stack exec -- ormolu -o '-XImportQualifiedPost' -o '-XPatternSynonyms' --mode check $(shell git ls-files '*.hs')

.PHONY: format format-check
