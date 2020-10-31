F =  # nothing by default

.PHONY: test

init:
	dune build @check

test:
	dune exec bin/test.exe -- test '$(F)'

ctest:
	dune exec bin/test.exe -- test '$(F)' -c

%.exe:
	dune build bin/$@

clean: clean-tests
	rm -Rf _build

clean-tests:
	find tests -name \*.s -type f -delete 
	find tests -name \*.o -type f -delete 
	find tests -name \*.run -type f -delete 
	find tests -name \*.result -type f -delete

