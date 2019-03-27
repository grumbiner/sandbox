
eniac :
	gcc -O2 -ansi $(@).c -o $(@)
	time ./eniac > ENIAC.OUT
	diff ENIAC.OUT ../reference/ENIAC.REF | wc
	ln -sf ../reference/ENIAC.REF .
	../check/check > delta.out 
