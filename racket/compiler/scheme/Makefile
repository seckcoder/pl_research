all:ctest
clean:
	rm -f stst *.s *.out test
scheme:
	gcc -O1 -o stst start.c stst.s
ctest:ctest.c
	clang -S ctest.c
	cat ctest.s
