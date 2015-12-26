.PHONY: clean test

clean:
	find . \( -name '*.hi' -or -name '*.o' \) -and -not -path '*/\.*' -delete
	stack clean
	cd examples && stack clean

test:
	stack test && cd examples && stack build
