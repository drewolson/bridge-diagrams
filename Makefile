stack = stack --allow-different-user

ifeq (${STACK_ENV}, CI)
build-options = --ghc-options=-O2
else
build-options =
endif

build:
	$(stack) build $(build-options)

test: build
	$(stack) test $(build-options)

.PHONY: build test
