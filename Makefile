all:
	stack build

.PHONY: test
test:
	stack test

.PHONY: clean
clean:
	stack clean
