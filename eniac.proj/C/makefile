
eniac :
	gcc -O2 -ansi $(@).c -o $(@)
	time ./eniac > eniac.out
	cmp -l eniac.out ../reference/ENIAC.OUT | wc
