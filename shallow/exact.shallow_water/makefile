all : shallow

#PROF=-fprofile-instr-generate
OPTS=-O2

shallow : shallow.C step.C
	mpic++ $(PROF) $(OPTS) $< -o $(@) -DSCALE=8 -I /Users/robertgrumbine/usrlocal/mmablib/include -I ~/usrlocal/include 
